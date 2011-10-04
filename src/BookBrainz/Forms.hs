{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module BookBrainz.Forms where

import           Data.Char                   (isDigit, digitToInt)
import           Control.Applicative         ((<$>), (<*>), pure)
import           Data.Maybe                  (fromMaybe)

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
import           BookBrainz.Model.Editor (getEditorByName)
import           BookBrainz.Types

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
       => Maybe [Int] -> SnapForm m Html BlazeFormHtml (Maybe Isbn)
isbn13 def = inputString ((concat . map show) `fmap` def)
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

bookForm :: (Monad m, MonadSnap m)
         => Maybe Book
         -> SnapForm m Html BlazeFormHtml Book
bookForm book = Book <$> simpleField "Book title:" (entityName $ bookName `fmap` book)

addEdition :: (MonadSnap m)
           => Ref (Concept Book)
           -> SnapForm m Html BlazeFormHtml Edition
addEdition book = Edition <$> simpleField "Name:" (entityName Nothing)
                          <*> pure Nothing
                          <*> pure book
                          <*> simpleField "Year:" (year Nothing)
                          <*> pure Nothing
                          <*> pure Nothing
                          <*> pure Nothing
                          <*> simpleField "ISBN:" (isbn13 Nothing)
                          <*> pure Nothing

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
