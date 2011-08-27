module BrainzStem.Model.Versioning
       ( findMasterBranch
       ) where

import Database.HDBC       (toSql)

import BrainzStem.Database ((!), HasDatabase, query)
import BrainzStem.Model    (HasTable (..), Entity (..), TableName (..)
                           ,InDatabase (..))
import BrainzStem.Types    (Branch (..), LoadedCoreEntity, LoadedEntity, gid)

instance HasTable Branch where
  tableName = TableName "bookbrainz_v.branch"
  newFromRow r = Branch { branchIsMaster = r ! "master"
                        , branchId = r ! "id"
                        }

instance Entity Branch
instance InDatabase Branch where
  rowKey = toSql . branchId

--------------------------------------------------------------------------------
-- | Find the master branch of a given entity.
findMasterBranch :: (HasDatabase m, Functor m)
                 => LoadedCoreEntity a
                 -> m (LoadedEntity Branch)
findMasterBranch ent = (entityFromRow . head) `fmap` query branchQuery
                                                        [ toSql $ gid ent ]
  where branchQuery = unlines [ "SELECT * FROM bookbrainz_v.branch"
                              , "WHERE gid = ? AND master = TRUE"
                              ]
