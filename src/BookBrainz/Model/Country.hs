-- | Functions for working with 'BookBrainz.Types.Country.Country' entities.
module BookBrainz.Model.Country where

import BrainzStem.Model (HasTable(..), Entity(..), TableName(..), Key(..), (!))
import BookBrainz.Types

instance HasTable Country where
  tableName = TableName "country"
  newFromRow row = Country { countryName = row ! "name"
                           , countryIsoCode = row ! "iso_code" }

instance Entity Country where
  key = Key "iso_code"
