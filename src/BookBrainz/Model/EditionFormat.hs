-- | Functions for working with 'BookBrainz.Types.Edition.EditionFormat'
-- entities.
module BookBrainz.Model.EditionFormat where

import Database.HDBC (toSql)

import BrainzStem.Database (query, Row)
import BrainzStem.Model (Entity(..), (!))
import BookBrainz.Types

instance Entity EditionFormat where
  getByPk pk = (fromRow . head) `fmap` query sql [ toSql pk ]
    where sql = "SELECT * FROM edition_format WHERE id = ?"
          fromRow r =
            Entity { entityInfo =
                       EditionFormat { editionFormatName = r ! "name" } }
