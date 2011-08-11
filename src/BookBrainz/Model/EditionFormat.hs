-- | Functions for working with 'BookBrainz.Types.Edition.EditionFormat'
-- entities.
module BookBrainz.Model.EditionFormat where

import BookBrainz.Model (HasTable(..), Entity(..), TableName(..), (!))
import BookBrainz.Types

instance HasTable EditionFormat where
  tableName = TableName "edition_format"
  newFromRow row = EditionFormat { editionFormatName = row ! "name" }

instance Entity EditionFormat
