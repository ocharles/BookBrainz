{-# LANGUAGE OverloadedStrings #-}

-- | Functions for working with 'BookBrainz.Types.Language.Language' entities.
module BookBrainz.Model.Language where

import Database.PostgreSQL.Simple (Only(..))
import Snap.Snaplet.PostgresqlSimple (HasPostgres, query, query_)

import BookBrainz.Types
import BrainzStem.Model (Entity(..))

instance Entity Language where
  getByPk pk = head `fmap` query sql (Only pk)
    where sql = "SELECT * FROM language WHERE iso_code = ?"

allLanguages :: (Functor m, HasPostgres m) => m [LoadedEntity Language]
allLanguages = query_ "SELECT * FROM language"
