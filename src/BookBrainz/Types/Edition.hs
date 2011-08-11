-- | Definition of an edition.
module BookBrainz.Types.Edition
  ( Edition(..)
  , EditionFormat(..)
  ) where

import Data.Text (Text)

import BookBrainz.Types.Book
import BookBrainz.Types.Country
import BookBrainz.Types.Language
import BookBrainz.Types.Publisher
import BrainzStem.Types

--------------------------------------------------------------------------------
-- | The actual format of the edition that people read.
data EditionFormat = EditionFormat
    { -- | The human-readable name of the format.
      editionFormatName :: Text
    } deriving Show

--------------------------------------------------------------------------------
-- | An edition is a release of a 'Book' that people actually read from.
data Edition = Edition
    { -- | The name of the edition.
      editionName        :: Text
      -- | A reference to the format of the edition.
    , editionFormat      :: Maybe (Ref EditionFormat)
      -- | The 'Book' that this edition is an edition of.
    , editionBook        :: Ref Book
      -- | The year the edition was released.
    , editionYear        :: Maybe Int
      -- | The 'Publisher' that published the edition.
    , editionPublisher   :: Maybe (Ref Publisher)
      -- | The 'Country' where the edition was published.
    , editionCountry     :: Maybe (Ref Country)
      -- | The 'Language' of this edition.
    , editionLanguage    :: Maybe (Ref Language)
      -- | The ISBN code of this edition.
    , editionIsbn        :: Maybe String
      {-| The barcode (EAN, UPC, etc.) of the edition. This is commonly the same
      as the ISBN, though there are cases when it is not. -}
    , editionBarcode     :: Maybe String
      -- | An index used for sorting this edition.
    , editionIndex       :: Maybe Int
    } deriving Show
