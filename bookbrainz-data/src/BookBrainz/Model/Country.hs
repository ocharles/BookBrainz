{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for working with 'BookBrainz.Types.Country.Country' entities.
module BookBrainz.Model.Country where

import Database.PostgreSQL.Simple (Only(..))
import Snap.Snaplet.PostgresqlSimple (HasPostgres, query, query_)

import BrainzStem.Model (Entity(..))
import BookBrainz.Types

instance Entity Country where
  getByPk pk = head `fmap` query sql (Only pk)
    where sql = "SELECT name, iso_code FROM country WHERE iso_code = ?"

allCountries :: (Functor m, HasPostgres m) => m [LoadedEntity Country]
allCountries = query_ "SELECT name, iso_code FROM country"
