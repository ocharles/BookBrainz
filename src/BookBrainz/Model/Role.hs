{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Functions for working with 'BookBrainz.Types.Role.Role' entities.
module BookBrainz.Model.Role
       ( HasRoles (..)
       ) where

import Database.HDBC (toSql)

import BrainzStem.Model.GenericVersioning (fromViewRow)
import BrainzStem.Database     (HasDatabase, prefixedRow, query, Row)
import BrainzStem.Model        (Entity(..), (!))
import BookBrainz.Model.Person ()
import BookBrainz.Types

instance Entity Role where
  getByPk pk = (fromRow . head) `fmap` query sql [ toSql pk ]
    where sql = "SELECT * FROM person_role WHERE id = ?"

fromRow :: Row -> LoadedEntity Role
fromRow r = Entity { entityInfo = Role { roleName = r ! "name" } }

--------------------------------------------------------------------------------
-- | The 'HasRoles' type class specifies that @entity@ has person-roles
-- associated with it.
class HasRoles entity where
  -- | Find all roles people played, in regards to a given entity.
  findRoles :: (HasDatabase m)
            => Ref (Tree entity)
            -- ^ The entity to find roles for.
            -> m [(LoadedEntity Role, LoadedCoreEntity Person)]
            -- ^ A list of (role, person) tuples.

instance HasRoles Book where
  findRoles = findRoles' "book"

instance HasRoles Edition where
  findRoles = findRoles' "edition"

-- Internal implementation with nasty string munging. Woohoo!
findRoles' :: (HasDatabase m, HasRoles roleLike)
           => String
           -> Ref (Tree roleLike)
           -> m [(LoadedEntity Role, LoadedCoreEntity Person)]
findRoles' tableName' treeId = do
  rows <- query roleSql [ rowKey treeId ]
  return $ personRoleFromRow `map` rows
  where roleSql =
          unlines [ "SELECT person.*, role.role_id AS r_id, role.name AS r_name"
                  , "FROM " ++ tableName' ++ "_person_role pr"
                  , "JOIN person_role role USING (role_id)"
                  , "JOIN person USING (person_id)"
                  , unwords ["JOIN", "bookbrainz_v." ++ tableName' ++ "_revision r", "USING", "(", tableName' ++ "_tree_id" ,")" ]
                  , "JOIN bookbrainz_v.branch ON branch.rev_id = r.rev_id"
                  , unwords ["JOIN", "bookbrainz_v." ++ tableName' ++ "_branch b", "ON", "branch.id", "=", "b.branch_id"]
                  , unwords ["WHERE",  tableName' ++ "_tree_id", "= ?"]
                  ]
        personRoleFromRow r =
          ( roleFromRow r
          , fromViewRow r
          )
        roleFromRow = fromRow . prefixedRow "r_"
