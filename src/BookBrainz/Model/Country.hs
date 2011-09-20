-- | Functions for working with 'BookBrainz.Types.Country.Country' entities.
module BookBrainz.Model.Country where

import Database.HDBC (toSql)

import BrainzStem.Database (query, Row)
import BrainzStem.Model (Entity(..), (!))
import BookBrainz.Types

instance Entity Country where
  getByPk pk = (fromRow . head) `fmap` query sql [ toSql pk ]
    where sql = "SELECT * FROM country WHERE iso_code = ?"
          fromRow r =
            Entity { entityInfo = Country { countryName = r ! "name"
                                          , countryIsoCode = r ! "iso_code" } }
