{-# LANGUAGE FlexibleInstances #-}

-- | Functions for working with 'BookBrainz.Types.Role.Role' entities.
module BookBrainz.Model.Role
       ( findRoles
       ) where

import Database.HDBC           (toSql)

import BrainzStem.Database     (HasDatabase, prefixedRow, query, (!))
import BrainzStem.Model        (Entity(..), coreEntityFromRow, HasTable(..)
                               ,TableName(..))
import BookBrainz.Model.Person ()
import BookBrainz.Types

instance HasTable Role where
  tableName = TableName "person_role"
  newFromRow row = Role { roleName = row ! "name" }

instance Entity Role

--------------------------------------------------------------------------------
-- | The 'HasRoles' type class specifies that @entity@ has person-roles
-- associated with it.
class HasRoles entity where
  -- | Find all roles people played, in regards to a given entity.
  findRoles :: HasDatabase m
            => LoadedCoreEntity entity
            -- ^ The entity to find roles for.
            -> m [(LoadedEntity Role, LoadedCoreEntity Person)]
            -- ^ A list of (role, person) tuples.

instance HasRoles Book where
  findRoles = findRoles' "book"

instance HasRoles Edition where
  findRoles = findRoles' "edition"

-- Internal implementation with nasty string munging. Woohoo!
findRoles' :: HasDatabase m
           => String -> LoadedCoreEntity a
           -> m [(LoadedEntity Role, LoadedCoreEntity Person)]
findRoles' tableName' ent = do
  rows <- query roleSql [ toSql $ coreEntityTree ent ]
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
          , coreEntityFromRow r :: LoadedCoreEntity Person
          )
        roleFromRow = entityFromRow . prefixedRow "r_"
