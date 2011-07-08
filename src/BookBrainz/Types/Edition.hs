module BookBrainz.Types.Edition
  ( Edition(..)
  , EditionFormat(..)
  ) where

import BookBrainz.Types.AuthorCredit
import BookBrainz.Types.Book
import BookBrainz.Types.Country
import BookBrainz.Types.Language
import BookBrainz.Types.Newtypes
import BookBrainz.Types.Person
import BookBrainz.Types.Publisher
import Data.Text (Text)
import Data.UUID (UUID)

data EditionFormat = EditionFormat
                     { editionFormatId :: EditionFormatId
                     , editionFormatName :: Text }
                   | PartialEditionFormat
                     { editionFormatId :: EditionFormatId
                     }
                   deriving Show

data Edition = Edition
               { editionName :: Text
               , editionGid :: UUID
               , editionId :: EditionId
               , editionFormat :: Maybe EditionFormat
               , editionBook :: Book
               , editionYear :: Maybe Int
               , editionPublisher :: Maybe Publisher
               , editionCountry :: Maybe Country
               , editionIllustrator :: Maybe Person
               , editionTranslator :: Maybe Person
               , editionAuthor :: AuthorCredit
               , editionLanguage :: Maybe Language
               , editionIsbn :: Maybe String
               , editionBarcode :: Maybe String
               , editionIndex :: Maybe Int
               }
             deriving Show
