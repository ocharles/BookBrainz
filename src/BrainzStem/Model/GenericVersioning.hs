{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BrainzStem.Model.GenericVersioning
       ( GenericallyVersioned (..)
       , VersionConfig (..)
       ) where

import Data.Maybe             (listToMaybe)

import Database.HDBC          (toSql, fromSql)

import BrainzStem.Database    (query, queryOne, (!), Row,
                              HasDatabase)
import BrainzStem.Model       (CoreEntity(..))
import BrainzStem.Types       (LoadedCoreEntity (..), LoadedEntity (..)
                              ,Revision (..), Ref (..))

data Version a
data VersionConfig a = VersionConfig { cfgView :: String
                                     , cfgIdCol :: String
                                     , cfgConcept :: String
                                     , cfgTree :: String
                                     , cfgBbid :: String
                                     , cfgRevision :: String
                                     , cfgBranch :: String
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
  getByBbid bbid' =
    (fmap fromViewRow . listToMaybe)
      `fmap` query selectSql [ toSql bbid' ]
    where selectSql = unlines [ "SELECT * FROM " ++ view
                              , "WHERE bbid = ?"
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

  newConcept bbid' = do
    concept <- createConcept
    attachBbid concept
    return $ fromSql concept
    where
      config = versioningConfig :: VersionConfig a
      createConcept =
            let conceptSql = unlines [ "INSERT INTO bookbrainz_v." ++
                                       (cfgConcept config)
                                     , "DEFAULT VALUES"
                                     , "RETURNING " ++ (cfgIdCol config)
                                     ]
            in
              queryOne conceptSql []
      attachBbid conceptRef =
            let attachSql = unlines [ "INSERT INTO bookbrainz_v." ++
                                      (cfgBbid config)
                                    , "(" ++ (cfgIdCol config) ++ ", bbid)"
                                    , "VALUES (?, ?)"
                                    ]
            in query attachSql [ conceptRef
                               , toSql bbid' ]

  newRevision baseTree pubData revId = do
    treeId <- case baseTree of
                Just _ -> error "Basing a revision off another not done"
                Nothing -> insertTree
    createRevision treeId
    return ()
    where
      config = versioningConfig :: VersionConfig a
      createRevision treeId =
        let pubRevSql = unlines [ "INSERT INTO bookbrainz_v." ++
                                  (cfgRevision config)
                                , "(rev_id, " ++ (cfgTree config) ++ "_id)"
                                , "VALUES (?, ?)"
                                ]
        in query pubRevSql [ toSql revId, treeId ]
      insertTree =
        let findOrInsertVersion = do
                foundId <- findVersion pubData
                case foundId of
                  Just id' -> return id'
                  Nothing -> newVersion pubData
            insertTreeSql = unlines
                    [ "INSERT INTO bookbrainz_v." ++
                      (cfgTree config)
                    , "(version) VALUES (?)"
                    , "RETURNING " ++ (cfgTree config) ++ "_id"
                    ]
        in do
          versionId <- findOrInsertVersion
          queryOne insertTreeSql [ toSql versionId ]

  attachBranchToConcept branch concept = do
    query branchSql [ toSql branch, toSql concept ]
    return ()
    where
      config = versioningConfig :: VersionConfig a
      branchSql = unlines [ "INSERT INTO bookbrainz_v." ++
                            (cfgBranch config)
                          , "(branch_id, " ++ (cfgIdCol config) ++ ")"
                          , "VALUES (?, ?)"
                          ]

  getRevision revision =
    (revisionFromRow . head) `fmap` query revSql [ toSql revision ]
    where
      config = versioningConfig :: VersionConfig a
      revSql = unlines [ "SELECT * FROM bookbrainz_v." ++
                         (cfgRevision config)
                       , "JOIN bookbrainz_v.revision USING (rev_id)"
                       , "WHERE rev_id = ?"
                       ]
      revisionFromRow row =
        Entity { entityInfo =
                    Revision { revisionId = row ! "rev_id"
                             , revisionTree = row ! ((cfgTree config) ++ "_id")
                             }
               }
