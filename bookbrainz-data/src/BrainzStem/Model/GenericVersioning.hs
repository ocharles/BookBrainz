{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BrainzStem.Model.GenericVersioning
       ( GenericallyVersioned (..)
       , VersionConfig (..)
       ) where

import Data.Maybe             (listToMaybe)
import Data.String            (fromString)

import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow(..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Snap.Snaplet.PostgresqlSimple      (execute, query, HasPostgres)

import BrainzStem.Database    (queryOne, queryOne_)
import BrainzStem.Model       (CoreEntity(..))
import BrainzStem.Types       (LoadedCoreEntity (..)
                              ,Revision (..), Branch (..), Ref, Tree, Concept)

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

  newTreeImpl :: (Functor m, HasPostgres m) => a -> m (Ref (Tree a))
  updateTreeImpl :: (Functor m, HasPostgres m) => a -> Ref (Tree a) -> m ()

instance ( GenericallyVersioned a, FromRow (LoadedCoreEntity a)
         , ToField (Ref (Concept a)), FromField (Ref (Concept a))
         , ToField (Ref (Revision a)), FromField (Ref (Revision a))
         , ToField (Ref (Tree a)), FromField (Ref (Branch a))
         , ToField (Ref (Branch a)), FromField (Ref (Tree a))
    ) => CoreEntity a where
  getByBbid bbid' =
    listToMaybe `fmap` query selectSql (Only bbid')
    where selectSql = fromString $ unlines [ "SELECT * FROM " ++ view
                              , "WHERE bbid = ?"
                              ]
          view = cfgView (versioningConfig :: VersionConfig a)

  getByConcept concept = head `fmap` query selectSql (Only concept)
    where selectSql = fromString $ unlines [ "SELECT * FROM " ++ view
                              , unwords ["WHERE", idCol, "= ?"]
                              ]
          view = cfgView (versioningConfig :: VersionConfig a)
          idCol = cfgIdCol (versioningConfig :: VersionConfig a)

  newTree = newTreeImpl
  updateTree = updateTreeImpl

  newConcept bbid' = do
    concept <- fmap (fromOnly . \x -> x `asTypeOf` (undefined :: Only (Ref (Concept a)))) createConcept
    attachBbid concept
    return concept
    where
      config = versioningConfig :: VersionConfig a
      createConcept =
            let conceptSql = fromString $ unlines [ "INSERT INTO bookbrainz_v." ++
                                       (cfgConcept config)
                                     , "DEFAULT VALUES"
                                     , "RETURNING " ++ (cfgIdCol config)
                                     ]
            in
              queryOne_ conceptSql
      attachBbid conceptRef =
            let attachSql = fromString $ unlines [ "INSERT INTO bookbrainz_v." ++
                                      (cfgBbid config)
                                    , "(" ++ (cfgIdCol config) ++ ", bbid)"
                                    , "VALUES (?, ?)"
                                    ]
            in execute attachSql (conceptRef, bbid')

  newRevision treeId revId =
    let pubRevSql = fromString $ unlines [ "INSERT INTO bookbrainz_v." ++
                              (cfgRevision config)
                            , "(rev_id, " ++ (cfgTree config) ++ "_id)"
                            , "VALUES (?, ?)"
                            , "RETURNING " ++ (cfgTree config) ++ "_id, rev_id"
                            ]
    in queryOne pubRevSql (revId, treeId)
    where
      config = versioningConfig :: VersionConfig a

  attachBranchToConcept branch concept = do
    execute branchSql (branch, concept)
    return ()
    where
      config = versioningConfig :: VersionConfig a
      branchSql = fromString $ unlines [ "INSERT INTO bookbrainz_v." ++
                            (cfgBranch config)
                          , "(branch_id, " ++ (cfgIdCol config) ++ ")"
                          , "VALUES (?, ?)"
                          ]

  getRevision revision = head `fmap` query revSql (Only revision)
    where
      config = versioningConfig :: VersionConfig a
      revSql = fromString $ unlines [ "SELECT * FROM bookbrainz_v." ++
                         (cfgRevision config)
                       , "JOIN bookbrainz_v.revision USING (rev_id)"
                       , "WHERE rev_id = ?"
                       ]

  findMasterBranch concept = head `fmap` query branchSql (Only concept)
    where
      config = versioningConfig :: VersionConfig a
      branchSql = fromString $ unlines [ "SELECT * FROM bookbrainz_v.branch"
                          , "JOIN bookbrainz_v." ++ (cfgBranch config)
                          , "USING (branch_id)"
                          , "WHERE " ++ (cfgConcept config) ++ "_id = ?"
                          , "AND master = TRUE"
                          ]
