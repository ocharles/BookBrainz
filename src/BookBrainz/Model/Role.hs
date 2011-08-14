{-# LANGUAGE FlexibleInstances #-}

-- | Functions for working with 'BookBrainz.Types.Role.Role' entities.
module BookBrainz.Model.Role
       ( findRoles
       ) where

import Database.HDBC           (toSql)

import BookBrainz.Database     (HasDatabase, prefixedRow, query, (!))
import BookBrainz.Model        (Entity(..), coreEntityFromRow, HasTable(..)
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
  findRoles book = do
    rows <- query roleSql [ toSql $ coreEntityVersion book ]
    return $ personRoleFromRow `map` rows
    where roleSql =
            unlines [ "SELECT person.*, bpr.role_id AS r_id, role.name AS r_name"
                    , "FROM book_person_role bpr"
                    , "JOIN person ON person.version = bpr.person"
                    , "JOIN person_role role USING (role_id)"
                    , "WHERE bpr.book = ?"
                    ]
          personRoleFromRow r =
            ( roleFromRow r
            , coreEntityFromRow r :: LoadedCoreEntity Person
            )
          roleFromRow = entityFromRow . prefixedRow "r_"
