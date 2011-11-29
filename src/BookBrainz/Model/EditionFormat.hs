-- | Functions for working with 'BookBrainz.Types.Edition.EditionFormat'
-- entities.
module BookBrainz.Model.EditionFormat
       ( allEditionFormats )
       where

import Database.HDBC (toSql)
import Snap.Snaplet.Hdbc (query, HasHdbc, Row)

import BrainzStem.Model (Entity(..), (!))
import BookBrainz.Types

instance Entity EditionFormat where
  getByPk pk = (fromRow . head) `fmap` query sql [ toSql pk ]
    where sql = "SELECT * FROM edition_format WHERE id = ?"

allEditionFormats :: (Functor m, HasHdbc m c s) => m [LoadedEntity EditionFormat]
allEditionFormats = map fromRow `fmap` query "SELECT * FROM edition_format" []

fromRow :: Row -> LoadedEntity EditionFormat
fromRow r = Entity { entityInfo =
                       EditionFormat { editionFormatName = r ! "name" }
                   , entityRef = r ! "id"
                   }
