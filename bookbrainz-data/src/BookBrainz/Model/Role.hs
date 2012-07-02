{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Functions for working with 'BookBrainz.Types.Role.Role' entities.
module BookBrainz.Model.Role
       ( HasRoles (..)
       , allRoles
       , copyRoles', findRoles', addRole'
       ) where

import Control.Monad (void)

import Data.Copointed (copoint)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.ToField
import Snap.Snaplet.PostgresqlSimple (HasPostgres, query, query_, execute)

import BrainzStem.Model.GenericVersioning (GenericallyVersioned)
import BrainzStem.Model        (Entity(..), Changes, revisionUnderChange)
import BookBrainz.Model.Person ()
import BookBrainz.Types

instance Entity Role where
  getByPk pk = head `fmap` query sql (Only pk)
    where sql = "SELECT * FROM person_role WHERE person_role_id = ?"

--------------------------------------------------------------------------------
-- | The 'HasRoles' type class specifies that @entity@ has person-roles
-- associated with it.
class HasRoles entity where
  -- | Find all roles people played, in regards to a given entity.
  findRoles :: (Functor m, HasPostgres m)
            => Ref (Tree entity)
            -- ^ The entity to find roles for.
            -> m [ LoadedEntity Role :. LoadedCoreEntity Person ]
            -- ^ A list of (role, person) tuples.

  copyRoles :: (Functor m, HasPostgres m)
            => Ref (Tree entity) -> Ref (Tree entity)
            -> m ()

  addRole :: (GenericallyVersioned entity)
          => (Ref (Concept Person), Ref Role)
          -> Changes entity ()

-- Internal implementation with nasty string munging. Woohoo!
findRoles' :: (HasPostgres m, HasRoles roleLike, ToField (Ref (Tree roleLike)))
           => String
           -> Ref (Tree roleLike)
           -> m [ LoadedEntity Role :. LoadedCoreEntity Person ]
findRoles' tableName' treeId = query roleSql (Only treeId)
  where roleSql = fromString $
          unlines [ "SELECT role.person_role_id AS r_role_id, role.name AS r_name, person.*"
                  , "FROM " ++ tableName' ++ "_person_role pr"
                  , "JOIN person_role role USING (role_id)"
                  , "JOIN person USING (person_id)"
                  , unwords ["JOIN", tableName' ++ "_revision r", "USING", "(", tableName' ++ "_tree_id" ,")" ]
                  , "JOIN branch ON branch.rev_id = r.rev_id"
                  , unwords ["JOIN", tableName' ++ "_branch b", "USING (branch_id)"]
                  , unwords ["WHERE",  tableName' ++ "_tree_id", "= ?"]
                  ]

copyRoles' :: (Functor m, HasPostgres m, HasRoles roleLike, ToField (Ref (Tree roleLike)))
           => String
           -> Ref (Tree roleLike) -> Ref (Tree roleLike)
           -> m ()
copyRoles' tableName' baseTreeId newTreeId =
  void $ execute sql (newTreeId, baseTreeId)
  where sql = let fullTable = tableName' ++ "_person_role"
                  col = tableName' ++ "_tree_id"
              in fromString $ unlines [ "INSERT INTO " ++ fullTable
                         , "(" ++ col ++ ", person_id, role_id)"
                         , "SELECT ?, person_id, role_id"
                         , "FROM " ++ fullTable
                         , "WHERE " ++ col ++ " = ?"
                         ]

addRole' :: (GenericallyVersioned entity, ToField (Ref (Concept Person)), ToField (Ref (Tree entity)))
         => String
         -> (Ref (Concept Person), Ref Role)
         -> Changes entity ()
addRole' tblName (person, role) = do
  r <- revisionUnderChange
  void $ execute addRoleSql (role, person, revisionTree $ copoint r)
  where
    addRoleSql = fromString $
      unlines [ "INSERT INTO " ++ tblName ++ "_person_role"
              , "(role_id, person_id, " ++ tblName ++ "_tree_id)"
              , "VALUES (?, ?, ?)"
              ]


--------------------------------------------------------------------------------
-- | Get all roles in the system.
allRoles :: (Functor m, HasPostgres m) => m [LoadedEntity Role]
allRoles = query_ "SELECT * FROM person_role"
