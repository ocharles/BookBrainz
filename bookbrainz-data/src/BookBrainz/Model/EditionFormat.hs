{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for working with 'BookBrainz.Types.Edition.EditionFormat'
-- entities.
module BookBrainz.Model.EditionFormat
       ( allEditionFormats )
       where

import Database.PostgreSQL.Simple (Only(..))
import Snap.Snaplet.PostgresqlSimple (query, query_, HasPostgres)

import BrainzStem.Model (Entity(..))
import BookBrainz.Types

instance Entity EditionFormat where
  getByPk pk = head `fmap` query sql (Only pk)
    where sql = "SELECT name, id FROM edition_format WHERE id = ?"

allEditionFormats :: (Functor m, HasPostgres m) => m [LoadedEntity EditionFormat]
allEditionFormats = query_ "SELECT name, id FROM edition_format"
