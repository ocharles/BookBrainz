-- | Functions for working with 'BookBrainz.Types.Language.Language' entities.
module BookBrainz.Model.Language where

import BookBrainz.Model (HasTable(..), Entity(..), TableName(..), Key(..), (!))
import BookBrainz.Types

instance HasTable Language where
  tableName = TableName "language"
  newFromRow row = Language { languageName = row ! "name"
                            , languageIsoCode = row ! "iso_code" }

instance Entity Language where
  key = Key "iso_code"
