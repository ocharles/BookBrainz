-- | Functions for working with 'BookBrainz.Types.Language.Language' entities.
module BookBrainz.Model.Language where

import Database.HDBC (toSql)

import BrainzStem.Database (query)
import BrainzStem.Model (Entity(..), (!))
import BookBrainz.Types

instance Entity Language where
  getByPk pk = (fromRow . head) `fmap` query sql [ toSql pk ]
    where sql = "SELECT * FROM language WHERE iso_code = ?"
          fromRow r =
            Entity { entityInfo =
                       Language { languageName = r ! "name"
                                , languageIsoCode = r ! "iso_code" } }
