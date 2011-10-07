-- | Functions for working with 'BookBrainz.Types.Publisher.Publisher' entities.
module BookBrainz.Model.Publisher
       ( allPublishers
       ) where

import Database.HDBC (fromSql, toSql)

import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))
import BookBrainz.Types                   (Publisher (..))
import BrainzStem.Database                ((!), queryOne, safeQueryOne
                                          ,HasDatabase, query)
import BrainzStem.Types                   (LoadedCoreEntity (..))

instance GenericallyVersioned Publisher where
  versioningConfig = VersionConfig { cfgView = "publisher"
                                   , cfgIdCol = "publisher_id"
                                   , cfgConcept = "publisher"
                                   , cfgTree = "publisher_tree"
                                   , cfgBbid = "publisher_bbid"
                                   , cfgRevision = "publisher_revision"
                                   , cfgBranch = "publisher_branch"
                                   }

  fromViewRow row =
    CoreEntity { bbid = row ! "bbid"
               , coreEntityRevision = row ! "rev_id"
               , coreEntityTree = row ! "publisher_tree_id"
               , coreEntityConcept = row ! "publisher_id"
               , coreEntityInfo = Publisher { publisherName = row ! "name" }
               }

  newTree _ pubData = do
    versionId <- findOrInsertVersion
    fromSql `fmap` queryOne insertTreeSql [ versionId ]
    where
      findOrInsertVersion = do
        foundId <- findVersion
        case foundId of
          Just id' -> return id'
          Nothing -> newVersion
      insertTreeSql = unlines [ "INSERT INTO bookbrainz_v.publisher_tree"
                              , "(version) VALUES (?)"
                              , "RETURNING publisher_tree_id"
                              ]
      findVersion =
        let findSql = unlines [ "SELECT version"
                              , "FROM bookbrainz_v.publisher_v"
                              , "WHERE name = ?"
                              ]
        in safeQueryOne findSql [ toSql $ publisherName pubData ]
      newVersion =
        let insertSql = unlines [ "INSERT INTO bookbrainz_v.publisher_v"
                                , "(name) VALUES (?)"
                                , "RETURNING version"
                                ]
        in queryOne insertSql [ toSql $ publisherName pubData ]

--------------------------------------------------------------------------------
-- | Get all publishers in the system
allPublishers :: HasDatabase m => m [LoadedCoreEntity Publisher]
allPublishers = map fromViewRow `fmap` query "SELECT * FROM publisher" []
