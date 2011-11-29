-- | Functions for working with 'BookBrainz.Types.Country.Country' entities.
module BookBrainz.Model.Country where

import Database.HDBC (toSql)
import Snap.Snaplet.Hdbc (HasHdbc, query, Row)

import BrainzStem.Model (Entity(..), (!))
import BookBrainz.Types

instance Entity Country where
  getByPk pk = (fromRow . head) `fmap` query sql [ toSql pk ]
    where sql = "SELECT * FROM country WHERE iso_code = ?"

allCountries :: (Functor m, HasHdbc m c s) => m [LoadedEntity Country]
allCountries = map fromRow `fmap` query "SELECT * FROM country" []

fromRow :: Row -> LoadedEntity Country
fromRow r = Entity { entityInfo = Country { countryName = r ! "name"
                                          , countryIsoCode = r ! "iso_code"
                                          }
                   , entityRef = r ! "iso_code"
                   }
