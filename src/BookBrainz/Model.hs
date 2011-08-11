{-# LANGUAGE ScopedTypeVariables #-}

{-| Common functions for accessing entities. -}
module BookBrainz.Model
       ( CoreEntity(..)
       , coreEntityFromRow
       , TableName(TableName)
       ) where

import Data.Maybe          (listToMaybe)

import Data.Map            ((!))
import Data.UUID           (UUID)
import Database.HDBC       (fromSql, toSql)

import BookBrainz.Database (HasDatabase, query, Row)
import BookBrainz.Types    (LoadedCoreEntity(..), Ref, rid)

newtype TableName a = TableName { getTableName :: String }

--------------------------------------------------------------------------------
{-| A "core" entity is an entity that has both a GID (BookBrainz identifier)
and is also versioned. This type class defines how they can be interacted with
via the database.

The minimal complete definition is 'tableName', and 'newFromRow'. -}
class CoreEntity a where
  {-| The name of this core entity's table in the PostgreSQL database.

  This has to wrapped in 'TableName' in order to make it polymorphic. -}
  tableName       :: TableName a

  {-| Create a new entity from the row in the database. 'coreEntityFromRow'
  will make use of this function, in order to create a 'LoadedCoreEntity'. -}
  newFromRow      :: Row -> a

  -- | Get a core entity by it's GID
  getByGid :: HasDatabase m
           => UUID
           -- ^ The GID of the core entity
           -> m (Maybe (LoadedCoreEntity a))
           -- ^ The 'LoadedCoreEntity' contextual representation of this core
           -- entity, or 'Nothing' if there was no entity with this GID. -}
  getByGid bbid = do
    results <- query selectQuery [ toSql bbid ]
    return $ coreEntityFromRow `fmap` listToMaybe results
    where table = getTableName (tableName :: TableName a)
          selectQuery = unlines  [ "SELECT *"
                                 , "FROM " ++ table
                                 , "WHERE gid = ?" ]

  -- | Get a specific version of this core entity. You have to use a 'Ref'
  -- here, because it's impossible to get a version of an entity without
  -- already knowing it's in the database.
  getVersion :: HasDatabase m
             => Ref (LoadedCoreEntity a)
             -- ^ A reference to the version of the core entity.
             -> m (LoadedCoreEntity a)
  getVersion version = do
    results <- query selectQuery [ toSql $ rid version ]
    return . coreEntityFromRow $ head results
    where table = getTableName (tableName :: TableName a)
          selectQuery = unlines  [ "SELECT *"
                                 , "FROM " ++ table
                                 , "WHERE version = ?" ]

--------------------------------------------------------------------------------
{-| Turn a 'Row' into a full 'LoadedCoreEntity'.

You should use this with care, as it is possible for a runtime exception here.
This could happen if the 'Row' map doesn't contain sufficient columns to create
the entity. -}
coreEntityFromRow :: CoreEntity a
                  => Row
                  -- ^ The 'Row' - from the result of a SELECT.
                  -> LoadedCoreEntity a
coreEntityFromRow row =
  CoreEntity { gid               = fromSql $ row ! "gid"
             , coreEntityVersion = fromSql $ row ! "version"
             , coreEntityInfo    = newFromRow row }
