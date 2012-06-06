{-# LANGUAGE OverloadedStrings #-}

-- | Functions for working with 'BookBrainz.Types.Publisher.Publisher' entities.
module BookBrainz.Model.Publisher
       ( allPublishers
       , publishedEditions
       ) where

import Data.String (fromString)
import Database.PostgreSQL.Simple (Only(..))
import Snap.Snaplet.PostgresqlSimple (HasPostgres, query, query_)

import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))
import BookBrainz.Model.Edition           ()
import BookBrainz.Types
import BrainzStem.Database (queryOne, safeQueryOne)

instance GenericallyVersioned Publisher where
  versioningConfig = VersionConfig { cfgView = "publisher"
                                   , cfgIdCol = "publisher_id"
                                   , cfgConcept = "publisher"
                                   , cfgTree = "publisher_tree"
                                   , cfgBbid = "publisher_bbid"
                                   , cfgRevision = "publisher_revision"
                                   , cfgBranch = "publisher_branch"
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
      insertTreeSql = fromString $ unlines [ "INSERT INTO bookbrainz_v.publisher_tree"
                              , "(version) VALUES (?)"
                              , "RETURNING publisher_tree_id"
                              ]
      findVersion =
        let findSql = fromString $ unlines [ "SELECT version"
                              , "FROM bookbrainz_v.publisher_v"
                              , "WHERE name = ?"
                              ]
        in (fmap fromOnly) `fmap` safeQueryOne findSql (Only $ publisherName pubData)
      newVersion =
        let insertSql = fromString $ unlines [ "INSERT INTO bookbrainz_v.publisher_v"
                                , "(name) VALUES (?)"
                                , "RETURNING version"
                                ]
        in (`asTypeOf` (undefined :: Int)) `fmap`
               (fmap fromOnly $ queryOne insertSql (Only $ publisherName pubData))

--------------------------------------------------------------------------------
-- | Get all publishers in the system
allPublishers :: (Functor m, HasPostgres m) => m [LoadedCoreEntity Publisher]
allPublishers = query_ "SELECT * FROM publisher"

--------------------------------------------------------------------------------
-- | Find all 'Edition's that this 'Publisher' published.
publishedEditions :: (Functor m, HasPostgres m)
                  => Ref (Concept Publisher)
                  -> m [LoadedCoreEntity Edition]
publishedEditions pubRef = query sql (Only pubRef)
  where sql = "SELECT * FROM edition WHERE publisher_id = ?"
