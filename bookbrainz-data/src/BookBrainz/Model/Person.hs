{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for working with 'BookBrainz.Types.Person.Person' entities.
module BookBrainz.Model.Person
       ( allPersons
       ) where

import Control.Applicative ((<$>))
import Data.String (fromString)
import Database.PostgreSQL.Simple (Only(..))
import Snap.Snaplet.PostgresqlSimple (HasPostgres, query_)

import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))

import BookBrainz.Types                   (Person (..))
import BrainzStem.Database                (queryOne, safeQueryOne)
import BrainzStem.Types                   (LoadedCoreEntity (..))

instance GenericallyVersioned Person where
  versioningConfig = VersionConfig { cfgView = "person"
                                   , cfgIdCol = "person_id"
                                   , cfgConcept = "person"
                                   , cfgTree = "person_tree"
                                   , cfgBbid = "person_bbid"
                                   , cfgRevision = "person_revision"
                                   , cfgBranch = "person_branch"
                                   }

  newTreeImpl pubData = do
    versionId <- findOrInsertVersion
    fromOnly `fmap` queryOne insertTreeSql (Only versionId)
   where
      findOrInsertVersion = do
        foundId <- findVersion
        case foundId of
          Just id' -> return id'
          Nothing -> newVersion
      insertTreeSql = fromString $ unlines [ "INSERT INTO person_tree"
                              , "(version) VALUES (?)"
                              , "RETURNING person_tree_id"
                              ]
      findVersion =
        let findSql = fromString $ unlines [ "SELECT person_data_id"
                              , "FROM person_data"
                              , "WHERE name = ?"
                              ]
        in (fmap fromOnly) `fmap` safeQueryOne findSql (Only $ personName pubData)
      newVersion =
        let insertSql = fromString $ unlines [ "INSERT INTO person_data"
                                , "(name) VALUES (?)"
                                , "RETURNING person_data_id"
                                ]
        in (\x -> x `asTypeOf` (undefined :: Int)) <$>
             (fmap fromOnly $ queryOne insertSql (Only $ personName pubData))


--------------------------------------------------------------------------------
-- | Get all persons in the system.
allPersons :: (Functor m, HasPostgres m) => m [LoadedCoreEntity Person]
allPersons = query_ "SELECT * FROM person"
