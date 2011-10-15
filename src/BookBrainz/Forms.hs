{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
module BookBrainz.Forms where

import           Data.Char                   (isDigit, digitToInt)
import           Control.Applicative         ((<$>), (<*>), pure)
import           Data.Maybe                  (fromMaybe, fromJust)

import           Data.Copointed              (copoint, Copointed)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Read              (decimal)
import           Snap.Core
import           Text.Blaze.Html5            (Html, (!), toValue, toHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive
import           Text.Digestive.Blaze.Html5
import           Text.Digestive.Forms        (FormInput(..))
import qualified Text.Digestive.Forms        as Forms
import           Text.Digestive.Forms.Snap

import           BrainzStem.Database (HasDatabase)
import           BookBrainz.Model.Country  (allCountries)
import           BookBrainz.Model.EditionFormat (allEditionFormats)
import           BookBrainz.Model.Language (allLanguages)
import           BookBrainz.Model.Editor (getEditorByName)
import           BookBrainz.Model.Person (allPersons)
import           BookBrainz.Model.Publisher (allPublishers)
import           BookBrainz.Model.Role (allRoles)
import           BookBrainz.Types (Book (Book), Edition (Edition)
                                  ,EditionFormat, Ref, Concept, Isbn
                                  ,Language, Country, Publisher (Publisher)
                                  ,Person (Person), Role
                                  ,entityRef, coreEntityConcept)
import qualified BookBrainz.Types as BB
import           BookBrainz.Web.Sitemap (showURL)
import qualified BookBrainz.Web.Sitemap as URL

data SearchQuery = SearchQuery { query :: Text }

data Login = Login { loginFormId :: Text
                   , loginFormPassword :: Text
                   , loginFormRemember :: Bool
                   }

data Registration = Registration { newUserName :: Text
                                 , newUserPassword :: Text
                                 , newUserPasswordConfirmation :: Text
                                 , newUserEmail :: Text
                                 }

--------------------------------------------------------------------------------
entityName :: (Monad m, MonadSnap m)
           => Maybe Text -> SnapForm m Html BlazeFormHtml Text
entityName def = inputText def `validate` nonEmpty "Name cannot be empty"

year :: (Monad m, MonadSnap m)
     => Maybe Int -> SnapForm m Html BlazeFormHtml (Maybe Int)
year def = inputText ((T.pack . show) `fmap` def)
             `validate` yearCheck
               `transform` transDecimal
  where yearCheck = check "Year field must only contain digits"
                      (or . zipWith ($) [T.all isDigit, T.null] . repeat)
        transDecimal = transformEither goTransform
        goTransform t | T.null t  = Right Nothing
                      | otherwise = case (decimal t :: Either String (Int, Text)) of
                                      Left e -> Left $ toHtml e
                                      Right (d, _) -> Right $ Just d

isbn13 :: (Monad m, MonadSnap m)
       => Maybe Isbn -> SnapForm m Html BlazeFormHtml (Maybe Isbn)
isbn13 def = inputString (show `fmap` def)
               `validate` checkIsbn `transform` transIsbn
  where
    checkIsbn = check "This is not a valid ISBN-13 identifier"
                      (or . zipWith ($) [null, validIsbn] . repeat)
    validIsbn i = let digits = map digitToInt i :: [Int]
                      isbn = init digits
                      checkDigit = last digits
                      checkSum (x:y:xs) = x + (3 * y) + checkSum xs
                      checkSum (_:[]) = error "Checksum must contain 12 digits"
                      checkSum [] = 0
                   in (all isDigit i) &&
                        (length i == 13) &&
                          ((10 - checkSum isbn `mod` 10) `mod` 10) == checkDigit
    transIsbn = transformEither goTransform
    goTransform t | null t    = Right Nothing
                  | otherwise = Right . Just $ read t

optionalDbSelect :: (Monad m, MonadSnap m, HasDatabase m, Eq a, Copointed cont)
                 => Maybe a
                 -> (cont b -> a)
                 -> (b -> Html)
                 -> [cont b]
                 -> SnapForm m Html BlazeFormHtml (Maybe a)
optionalDbSelect def fVal fLabel options =
  inputSelect def $ [(Nothing,"")] ++ map fOption options
  where fOption v = (Just . fVal $ v, fLabel . copoint $ v)

justDbSelect :: (Monad m, MonadSnap m, HasDatabase m, Eq a, Copointed cont)
             => Maybe a
             -> (cont b -> a)
             -> (b -> Html)
             -> [cont b]
             -> SnapForm m Html BlazeFormHtml a
justDbSelect def fVal fLabel options =
  fromJust <$> inputSelect def (map fOption options)
  where fOption v = (Just . fVal $ v, fLabel . copoint $ v)

editionFormat :: (Monad m, MonadSnap m, HasDatabase m)
              => Maybe (Ref EditionFormat)
              -> m (SnapForm m Html BlazeFormHtml (Maybe (Ref EditionFormat)))
editionFormat def =
  optionalDbSelect def entityRef
                       (toHtml . BB.editionFormatName)
    <$> allEditionFormats

language :: (Monad m, MonadSnap m, HasDatabase m)
         => Maybe (Ref Language)
         -> m (SnapForm m Html BlazeFormHtml (Maybe (Ref Language)))
language def =
  optionalDbSelect def entityRef
                       (toHtml . BB.languageName)
    <$> allLanguages

country :: (Monad m, MonadSnap m, HasDatabase m)
        => Maybe (Ref Country)
        -> m (SnapForm m Html BlazeFormHtml (Maybe (Ref Country)))
country def =
  optionalDbSelect def entityRef
                       (toHtml . BB.countryName)
    <$> allCountries

publisherRef :: (Monad m, MonadSnap m, HasDatabase m)
             => Maybe (Ref (Concept Publisher))
             -> m (SnapForm m Html BlazeFormHtml (Maybe (Ref (Concept Publisher))))
publisherRef def = do
  opts <- allPublishers
  return (buildField opts <++ viewHtml addNew)
  where
    buildField opts =
      optionalDbSelect def coreEntityConcept (toHtml . BB.publisherName) opts
    addNew = H.a ! A.target "_blank"
                 ! A.href (toValue . showURL $ URL.AddPublisher) $
               "Add a new publisher"

personRef :: (Monad m, MonadSnap m, HasDatabase m)
             => Maybe (Ref (Concept Person))
             -> m (SnapForm m Html BlazeFormHtml (Maybe (Ref (Concept Person))))
personRef def = do
  opts <- allPersons
  return (buildField opts <++ viewHtml addNew)
  where
    buildField opts =
      optionalDbSelect def coreEntityConcept (toHtml . BB.personName) opts
    addNew = H.a ! A.target "_blank"
                 ! A.href (toValue . showURL $ URL.AddPerson) $
               "Add a new person"

justPersonRef :: (Monad m, MonadSnap m, HasDatabase m)
              => Maybe (Ref (Concept Person))
              -> m (SnapForm m Html BlazeFormHtml (Ref (Concept Person)))
justPersonRef def = do
  opts <- allPersons
  return (buildField opts <++ viewHtml addNew)
  where
    buildField opts =
      justDbSelect def coreEntityConcept (toHtml . BB.personName) opts
    addNew = H.a ! A.target "_blank"
                 ! A.href (toValue . showURL $ URL.AddPerson) $
               "Add a new person"

role :: (Monad m, MonadSnap m, HasDatabase m)
         => Maybe (Ref Role)
         -> m (SnapForm m Html BlazeFormHtml (Ref Role))
role def =
  justDbSelect def entityRef (toHtml . BB.roleName)
    <$> allRoles

bookForm :: (Monad m, MonadSnap m)
         => Maybe Book
         -> SnapForm m Html BlazeFormHtml Book
bookForm book = Book <$> simpleField "Book title:" (entityName $ BB.bookName `fmap` book)

addEdition :: (MonadSnap m, HasDatabase m)
           => Ref (Concept Book)
           -> m (SnapForm m Html BlazeFormHtml Edition)
addEdition book = do
  formatField    <- editionFormat Nothing
  countryField   <- country Nothing
  languageField  <- language Nothing
  publisherField <- publisherRef Nothing
  return $
    Edition <$> simpleField "Name:" (entityName Nothing)
            <*> simpleField "Format:" formatField
            <*> pure book
            <*> simpleField "Year:" (year Nothing)
            <*> simpleField "Publisher:" publisherField
            <*> simpleField "Country:" countryField
            <*> simpleField "Language:" languageField
            <*> simpleField "ISBN:" (isbn13 Nothing)
            <*> pure Nothing

editEdition :: (MonadSnap m, HasDatabase m)
            => Edition
            -> m (SnapForm m Html BlazeFormHtml Edition)
editEdition edition = do
  formatField    <- editionFormat $ BB.editionFormat edition
  countryField   <- country       $ BB.editionCountry edition
  languageField  <- language      $ BB.editionLanguage edition
  publisherField <- publisherRef  $ BB.editionPublisher edition
  return $
    Edition <$> simpleField "Name:" (entityName . Just $ BB.editionName edition)
            <*> simpleField "Format:" formatField
            <*> pure (BB.editionBook edition)
            <*> simpleField "Year:" (year $ BB.editionYear edition)
            <*> simpleField "Publisher:" publisherField
            <*> simpleField "Country:" countryField
            <*> simpleField "Language:" languageField
            <*> simpleField "ISBN:" (isbn13 $ BB.editionIsbn edition)
            <*> pure Nothing

addPublisher :: (MonadSnap m)
             => SnapForm m Html BlazeFormHtml Publisher
addPublisher = Publisher <$> simpleField "Name:" (entityName Nothing)

addPerson :: (MonadSnap m)
          => SnapForm m Html BlazeFormHtml Person
addPerson = Person <$> simpleField "Name:" (entityName Nothing)

personRole :: (HasDatabase m, MonadSnap m)
           => m (SnapForm m Html BlazeFormHtml (Ref (Concept Person), Ref Role))
personRole = do
  personField <- justPersonRef Nothing
  roleField   <- role Nothing
  return $
    (,) <$> simpleField "Person:" personField
        <*> simpleField "Role:" roleField

--------------------------------------------------------------------------------
searchForm :: (Monad m, MonadSnap m) => SnapForm m Html BlazeFormHtml SearchQuery
searchForm = SearchQuery <$> simpleField "Query: "
                               (inputText Nothing `validate` nonEmpty "Query cannot be empty")

--------------------------------------------------------------------------------
userName :: (Monad m, MonadSnap m)
         => Maybe Text -> SnapForm m Html BlazeFormHtml Text
userName inp = inputText inp `validate` nonEmpty "Please enter a username"

password :: (Monad m, MonadSnap m)
         => Maybe Text -> SnapForm m Html BlazeFormHtml Text
password inp = inputPass inp `validate` nonEmpty "Please enter a password"

registerForm :: (Monad m, MonadSnap m, HasDatabase m)
             => SnapForm m Html BlazeFormHtml Registration
registerForm = (`transform` newUserCheck) $ (`validate` passwordsMatch) $ (errors ++>) $
                  Registration <$> simpleField "User name:" (userName Nothing)
                               <*> simpleField "Password:" (password Nothing)
                               <*> simpleField "Confirm password:" (password Nothing)
                               <*> simpleField "Email address:"
                                     (inputText Nothing)
  where passwordsMatch = check "Passwords must match" $ \reg ->
          newUserPassword reg == newUserPasswordConfirmation reg
        newUserCheck = transformEitherM $ \submission -> do
          editor <- getEditorByName $ newUserName submission
          return $ case editor of
            Just _  -> Left "An account with this name already exists"
            Nothing -> Right submission

loginForm :: (Monad m, MonadSnap m)
          => SnapForm m Html BlazeFormHtml Login
loginForm = Login <$> simpleField "User name:"
                        (inputText Nothing `validate` nonEmpty "Please enter a username")
                  <*> simpleField "Password:"
                        (inputPass Nothing `validate` nonEmpty "Please enter a password")
                  <*> mapViewHtml H.p (inputCheckBox False <++ label "Remember me")

--------------------------------------------------------------------------------                  
nonEmpty :: Monad m => Html -> Validator m Html Text
nonEmpty message = check message (not . T.null)

--------------------------------------------------------------------------------
simpleField :: (Monad m, MonadSnap m)
            => String
            -> SnapForm m Html BlazeFormHtml a
            -> SnapForm m Html BlazeFormHtml a
simpleField labelText field = mapViewHtml H.p (label labelText ++> field <++ errors)

--------------------------------------------------------------------------------
inputPass :: (Monad m, Functor m, FormInput i f)
          => Formlet m i e BlazeFormHtml Text
inputPass = Forms.inputText $ \id' inp -> createFormHtml $ \_ ->
  H.input ! A.type_ "password"
          ! A.name (toValue $ show id')
          ! A.id (toValue $ show id')
          ! A.value (toValue $ fromMaybe "" inp)

--------------------------------------------------------------------------------          
processForm :: (MonadSnap m)
            => SnapForm m e v a  -- ^ Form
            -> String            -- ^ Form name
            -> m (Either v a)    -- ^ Result
processForm form name = eitherForm form name snapEnvironment
