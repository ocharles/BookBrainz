{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
module BookBrainz.Forms where

import           Data.Char                   (isDigit, digitToInt)
import           Control.Applicative         ((<$>), (<*>), pure)
import           Data.Maybe                  (isNothing)

import           Data.Copointed              (copoint, Copointed)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Read              (decimal)
import           Snap.Core
import Snap.Snaplet.PostgresqlSimple (HasPostgres)
import           Text.Blaze.Html5            (Html, toHtml)
import           Text.Digestive
import           Text.Digestive.Snap

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

data SearchQuery = SearchQuery { query :: Text }

data Login = Login { loginFormId :: Text
                   , loginFormPassword :: Text
                   , loginFormRemember :: Bool
                   }

data Registration = Registration { newUserName :: Text
                                 , newUserPassword :: Text
                                 , newUserEmail :: Text
                                 }

bookForm :: (Monad m, MonadSnap m)
         => Maybe Book
         -> Form Html m Book
bookForm book = Book <$> "title" .: nonEmptyText (BB.bookName `fmap` book)

personRole :: (HasPostgres m, MonadSnap m)
           => m (Form Html m (Ref (Concept Person), Ref Role))
personRole = do
  personField <- justPersonRef Nothing
  roleField   <- role Nothing
  return $
    (,) <$> "person" .: personField
        <*> "role" .: roleField

justPersonRef :: (Monad m, MonadSnap m, HasPostgres m)
              => Maybe (Ref (Concept Person))
              -> m (Form Html m (Ref (Concept Person)))
justPersonRef def = do
  opts <- allPersons
  return $ choice
    (map (\a -> (coreEntityConcept a, toHtml $ BB.personName $ copoint a)) opts) def

role :: (Monad m, MonadSnap m, HasPostgres m)
     => Maybe (Ref Role)
     -> m (Form Html m (Ref Role))
role def = do
  opts <- allRoles
  return $ choice (map (\a -> (entityRef a, toHtml $ BB.roleName $ copoint a)) opts) def

addEdition :: (MonadSnap m, HasPostgres m)
           => Ref (Concept Book)
           -> m (Form Html m Edition)
addEdition book = do
  formatField    <- editionFormat Nothing
  countryField   <- country Nothing
  languageField  <- language Nothing
  publisherField <- publisherRef Nothing
  return $
    Edition <$> "name" .: nonEmptyText Nothing
            <*> "format" .: formatField
            <*> pure book
            <*> "year" .: year Nothing
            <*> "publisher" .: publisherField
            <*> "country" .: countryField
            <*> "language" .: languageField
            <*> "isbn" .: isbn13 Nothing
            <*> pure Nothing

editEdition :: (MonadSnap m, HasPostgres m)
            => Edition
            -> m (Form Html m Edition)
editEdition edition = do
  formatField    <- editionFormat $ BB.editionFormat edition
  countryField   <- country       $ BB.editionCountry edition
  languageField  <- language      $ BB.editionLanguage edition
  publisherField <- publisherRef  $ BB.editionPublisher edition
  return $
    Edition <$> "name" .: (nonEmptyText . Just $ BB.editionName edition)
            <*> "format" .: formatField
            <*> pure (BB.editionBook edition)
            <*> "year" .: year (BB.editionYear edition)
            <*> "publisher" .: publisherField
            <*> "country" .: countryField
            <*> "language" .: languageField
            <*> "isbn" .: isbn13 (BB.editionIsbn edition)
            <*> pure Nothing

language :: (Monad m, MonadSnap m, HasPostgres m)
         => Maybe (Ref Language)
         -> m (Form Html m (Maybe (Ref Language)))
language def = do
  opts <- allLanguages
  return $ choice ((Nothing, "") : map (\a -> (Just $ entityRef a, toHtml $ BB.languageName $ copoint a)) opts) (fmap Just def)

editionFormat :: (Monad m, MonadSnap m, HasPostgres m)
              => Maybe (Ref EditionFormat)
              -> m (Form Html m (Maybe (Ref EditionFormat)))
editionFormat def = do
  opts <- allEditionFormats
  return $ choice ((Nothing, "") : map (\a -> (Just $ entityRef a, toHtml $ BB.editionFormatName $ copoint a)) opts) (fmap Just def)

country :: (Monad m, MonadSnap m, HasPostgres m)
        => Maybe (Ref Country)
        -> m (Form Html m (Maybe (Ref Country)))
country def = do
  opts <- allCountries
  return $ choice ((Nothing, "") : map (\a -> (Just $ entityRef a, toHtml $ BB.countryName $ copoint a)) opts) (fmap Just def)

publisherRef :: (Monad m, MonadSnap m, HasPostgres m)
             => Maybe (Ref (Concept Publisher))
             -> m (Form Html m (Maybe (Ref (Concept Publisher))))
publisherRef def = do
  opts <- allPublishers
  return $ choice
    ((Nothing, "") : map (\a -> (Just $ coreEntityConcept a, toHtml $ BB.publisherName $ copoint a)) opts) (fmap Just def)

year :: (Monad m, MonadSnap m)
     => Maybe Int -> Form Html m (Maybe Int)
year def = validate yearCheck $ text ((T.pack . show) `fmap` def)
  where
    yearCheck y | T.null y = Success Nothing
                | otherwise   =
      if T.all isDigit y
      then case (decimal y :: Either String (Int, Text)) of
        Left e -> Error $ toHtml e
        Right (d, _) -> Success $ Just d
      else Error "A year can only consist of integers"

isbn13 :: (Monad m, MonadSnap m)
       => Maybe Isbn -> Form Html m (Maybe Isbn)
isbn13 def = validate checkIsbn $ text ((T.pack . show) `fmap` def)
  where
    checkIsbn isbn | T.null isbn    = Success Nothing
                   | validIsbn isbn = Success $ read $ T.unpack isbn
                   | otherwise      = Error "This is not a valid ISBN-13 identifier"
    validIsbn i = let digits = map digitToInt (T.unpack i) :: [Int]
                      isbn = init digits
                      checkDigit = last digits
                      checkSum (x:y:xs) = x + (3 * y) + checkSum xs
                      checkSum (_:[]) = error "Checksum must contain 12 digits"
                      checkSum [] = 0
                   in all isDigit (T.unpack i) &&
                        (T.length i == 13) &&
                          ((10 - checkSum isbn `mod` 10) `mod` 10) == checkDigit

addPerson :: (MonadSnap m)
          => Form Html m Person
addPerson = Person <$> "name" .: nonEmptyText Nothing

--------------------------------------------------------------------------------
addPublisher :: (MonadSnap m)
             => Form Html m Publisher
addPublisher = Publisher <$> "name" .: nonEmptyText Nothing

--------------------------------------------------------------------------------
searchForm :: (Monad m, MonadSnap m) => Form Html m SearchQuery
searchForm = SearchQuery <$> "query" .: nonEmptyText Nothing

--------------------------------------------------------------------------------
registerForm :: (Monad m, MonadSnap m, HasPostgres m)
             => Form Html m Registration
registerForm = Registration <$> "userName" .: nonExistingUser
                            <*> "password" .: passwordForm
                            <*> "email" .: nonEmptyText Nothing
  where
    passwordForm = validate matchPasswords $
                     (,) <$> "password" .: nonEmptyText Nothing
                         <*> "confirmPassword" .: nonEmptyText Nothing
    matchPasswords (a, b) | a == b    = Success a
                          | otherwise = Error "Error passwords must match"
    nonExistingUser = checkM "An account with this name already exists"
      (fmap isNothing . getEditorByName) $ nonEmptyText Nothing

loginForm :: (Monad m, MonadSnap m) => Form Html m Login
loginForm = Login <$> "userName" .: nonEmptyText Nothing
                  <*> "password" .: nonEmptyText Nothing
                  <*> "remember" .: bool Nothing

--------------------------------------------------------------------------------
nonEmptyText :: (Monad m, MonadSnap m) => Formlet Html m Text
nonEmptyText = check "Cannot be empty" (not . T.null) . text

--------------------------------------------------------------------------------
processForm :: (MonadSnap m)
            => Form v m a           -- ^ Form
            -> Text                 -- ^ Form name
            -> m (View v, Maybe a)  -- ^ Result
processForm form name = runForm name form
