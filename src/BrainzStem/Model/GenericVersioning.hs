{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BrainzStem.Model.GenericVersioning
       ( GenericallyVersioned (..)
       , VersionConfig (..)
       ) where

import Data.Maybe             (listToMaybe)

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.UUID              (UUID)
import Database.HDBC          (toSql, fromSql)
import System.Random          (randomIO)

import BrainzStem.Database    (query, queryOne, (!), Row,
                              HasDatabase)
import BrainzStem.Model       (CoreEntity(..))
import BrainzStem.Types       (LoadedCoreEntity (..), LoadedEntity (..)
                              ,Revision (..), Ref (..))

data Version a
data VersionConfig a = VersionConfig { cfgView :: String
                                     , cfgIdCol :: String
                                     }

--------------------------------------------------------------------------------
-- | A simplified type class to create an instance of, to provide instances of
-- 'CoreEntity',
class GenericallyVersioned a where
  -- | The versioning "configuration" for a given entity type. See
  -- 'VersionConfig'. for what this really entails.
  versioningConfig :: VersionConfig a

  -- | Create a 'LoadedCoreEntity' from a row from the unified view.
  fromViewRow :: Row -> LoadedCoreEntity a

  -- | Find an existing version row, from the definition of an entity.
  findVersion :: HasDatabase m
              => a
              -> m (Maybe (Ref (Version a)))

  -- | Insert a new version row, returning a 'Ref' to it. Called if
  -- 'findVersion' returns 'Nothing'.
  newVersion :: HasDatabase m
             => a
             -> m (Ref (Version a))

instance GenericallyVersioned a => CoreEntity a where
  getByGid bbid =
    (fmap fromViewRow . listToMaybe)
      `fmap` query selectSql [ toSql bbid ]
    where selectSql = unlines [ "SELECT * FROM " ++ view
                              , "WHERE gid = ?"
                              ]
          view = cfgView (versioningConfig :: VersionConfig a)

  getByConcept concept =
    (fromViewRow . head)
      `fmap` query selectSql [ toSql concept ]
    where selectSql = unlines [ "SELECT * FROM " ++ view
                              , unwords ["WHERE", idCol, "= ?"]
                              ]
          view = cfgView (versioningConfig :: VersionConfig a)
          idCol = cfgIdCol (versioningConfig :: VersionConfig a)

  newConcept = do
    concept <- createConcept
    attachGid concept
    return $ fromSql concept
    where
      createConcept =
            let conceptSql = unlines [ "INSERT INTO bookbrainz_v.publisher"
                                     , "DEFAULT VALUES"
                                     , "RETURNING publisher_id"
                                     ]
            in
              queryOne conceptSql []
      attachGid conceptRef =
            let generateGid = liftIO randomIO :: MonadIO m => m UUID
                attachToConcept uuid = query attachSql [ conceptRef
                                                       , toSql uuid ]
                attachSql = unlines [ "INSERT INTO bookbrainz_v.publisher_gid"
                                    , "(publisher_id, gid) VALUES (?, ?)"
                                    ]
            in generateGid >>= attachToConcept

  newRevision baseTree pubData revId = do
    treeId <- case baseTree of
                Just _ -> error "Basing a revision off another not done"
                Nothing -> insertTree
    createRevision treeId
    return ()
    where
      createRevision treeId =
        let pubRevSql = unlines [ "INSERT INTO bookbrainz_v.publisher_revision"
                                , "(rev_id, publisher_id) VALUES (?, ?)"
                                ]
        in query pubRevSql [ toSql revId, treeId ]
      insertTree =
        let findOrInsertVersion =do
                foundId <- findVersion pubData
                case foundId of
                  Just id' -> return id'
                  Nothing -> newVersion pubData
            insertTreeSql = unlines
                    [ "INSERT INTO bookbrainz_v.publisher_tree"
                    , "(version) VALUES (?)"
                    , "RETURNING tree_id"
                    ]
        in do
          versionId <- findOrInsertVersion
          queryOne insertTreeSql [ toSql versionId ]

  attachBranchToConcept branch concept = do
    query branchSql [ toSql branch, toSql concept ]
    return ()
    where branchSql = unlines [ "INSERT INTO bookbrainz_v.publisher_branch"
                              , "(branch_id, publisher_id) VALUES (?, ?)"
                              ]

  getRevision revision =
    (revisionFromRow . head) `fmap` query revSql [ toSql revision ]
    where revSql = unlines [ "SELECT * FROM bookbrainz_v.publisher_revision"
                           , "JOIN bookbrainz_v.revision USING (rev_id)"
                           , "WHERE rev_id = ?"
                           ]
          revisionFromRow row =
            Entity { entityInfo =
                       Revision { revisionId = row ! "rev_id"
                                , revisionTree = row ! "publisher_tree_id"
                                }
                   }
