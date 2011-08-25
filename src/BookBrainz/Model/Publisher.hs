-- | Functions for working with 'BookBrainz.Types.Publisher.Publisher' entities.
module BookBrainz.Model.Publisher where

import BrainzStem.Model    (CoreEntity(..), HasTable(..)
                           ,TableName(..), (!))
import BookBrainz.Types

--------------------------------------------------------------------------------
instance HasTable Publisher where
  tableName = TableName "publisher"
  newFromRow row =
    Publisher { publisherName = row ! "name"
              }

instance CoreEntity Publisher where
