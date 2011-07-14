module BookBrainz.Types.Edition
  ( Edition(..)
  , EditionFormat(..)
  ) where

import BookBrainz.Types.AuthorCredit
import BookBrainz.Types.Book
import BookBrainz.Types.Country
import BookBrainz.Types.Language
import BookBrainz.Types.Person
import BookBrainz.Types.Publisher
import BrainzStem.Types
import Data.Text (Text)

data EditionFormat = EditionFormat
                     { editionFormatId :: Int
                     , editionFormatName :: Text
                     }
                   deriving Show

data Edition = Edition
               { editionName        :: Text
               , editionFormat      :: Maybe (Ref EditionFormat)
               , editionBook        :: Ref (LoadedCoreEntity Book)
               , editionYear        :: Maybe Int
               , editionPublisher   :: Maybe (Ref Publisher)
               , editionCountry     :: Maybe (Ref Country)
               , editionIllustrator :: Maybe (Ref Person)
               , editionTranslator  :: Maybe (Ref Person)
               , editionAuthor      :: Ref AuthorCredit
               , editionLanguage    :: Maybe (Ref Language)
               , editionIsbn        :: Maybe String
               , editionBarcode     :: Maybe String
               , editionIndex       :: Maybe Int
               }
             deriving Show
