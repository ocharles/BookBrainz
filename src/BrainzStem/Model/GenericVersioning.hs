{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BrainzStem.Model.GenericVersioning
       ( GenericallyVersioned (..)
       , VersionConfig (..)
       ) where

import Control.Applicative (Applicative)
import Data.Maybe             (listToMaybe)

import Database.HDBC          (toSql, fromSql)
import Snap.Snaplet.Hdbc      (query, Row, HasHdbc)

import BrainzStem.Database    (queryOne, (!))
import BrainzStem.Model       (CoreEntity(..))
import BrainzStem.Types       (LoadedCoreEntity (..), LoadedEntity (..)
                              ,Revision (..), Ref (..), Branch (..), Tree)

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

  -- | Create a new 'Tree' for this @a@, optionally basing on an existing
  -- tree.
  newTree :: (Applicative m, Functor m, HasHdbc m c s)
          => Maybe (Ref (Tree a))
          -> a
          -> m (Ref (Tree a))

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

  newRevision baseTree entData revId =
    newTree baseTree entData >>= newRevision'
    where
      config = versioningConfig :: VersionConfig a
      newRevision' treeId =
        let pubRevSql = unlines [ "INSERT INTO bookbrainz_v." ++
                                  (cfgRevision config)
                                , "(rev_id, " ++ (cfgTree config) ++ "_id)"
                                , "VALUES (?, ?)"
                                ]
            revFromRow _ =
              Entity { entityInfo = Revision { revisionTree = treeId }
                     , entityRef = revId
                     }
        in revFromRow `fmap` queryOne pubRevSql [ toSql revId, toSql treeId ]

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
                    Revision { revisionTree = row ! ((cfgTree config) ++ "_id")
                             }
               , entityRef = row ! "rev_id"
               }

  findMasterBranch concept =
    (branchFromRow . head) `fmap` query branchSql [ toSql concept ]
    where
      config = versioningConfig :: VersionConfig a
      branchSql = unlines [ "SELECT * FROM bookbrainz_v.branch"
                          , "JOIN bookbrainz_v." ++ (cfgBranch config)
                          , "USING (branch_id)"
                          , "WHERE " ++ (cfgConcept config) ++ "_id = ?"
                          , "AND master = TRUE"
                          ]
      branchFromRow row =
        Entity { entityInfo = Branch { branchIsMaster = row ! "master"
                                     , branchConcept = row ! (cfgConcept config)
                                     , branchRevision = row ! "rev_id"
                                     }
               , entityRef = row ! "branch_id"
               }
