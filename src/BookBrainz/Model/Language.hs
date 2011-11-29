-- | Functions for working with 'BookBrainz.Types.Language.Language' entities.
module BookBrainz.Model.Language where

import Database.HDBC (toSql)
import Snap.Snaplet.Hdbc (HasHdbc, Row, query)

import BookBrainz.Types
import BrainzStem.Database ((!))
import BrainzStem.Model (Entity(..))

instance Entity Language where
  getByPk pk = (fromRow . head) `fmap` query sql [ toSql pk ]
    where sql = "SELECT * FROM language WHERE iso_code = ?"

allLanguages :: (Functor m, HasHdbc m c s) => m [LoadedEntity Language]
allLanguages = map fromRow `fmap` query "SELECT * FROM language" []

fromRow :: Row -> LoadedEntity Language
fromRow r = Entity { entityInfo =
                       Language { languageName = r ! "name"
                                , languageIsoCode = r ! "iso_code"
                                }
                   , entityRef = r ! "iso_code"
                   }
