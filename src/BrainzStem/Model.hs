{-# LANGUAGE ScopedTypeVariables #-}

{-| Common functions for accessing entities. -}
module BrainzStem.Model
       (
         -- * Entity Manipulation
         CoreEntity(..)
       , Entity(..)

         -- * Entity Definition
       , HasTable(..)
       , InDatabase(..)

         -- * Low Level Functions and Types
       , TableName(TableName)
       , Key(Key)
       , (!)
       ) where

import Data.Maybe          (listToMaybe)

import Data.Copointed      (copoint)
import Data.UUID           (UUID)
import Database.HDBC       (SqlValue, toSql)

import BrainzStem.Database (HasDatabase, query, Row, (!))
import BrainzStem.Types    (LoadedCoreEntity(..), LoadedEntity(..), Ref(..))

{-| Represents the table name for an entity. @a@ is the type of entity this
is a table name for. -}
newtype TableName a = TableName { getTableName :: String }

{-| Represents the name of the primary key column for an entity. @a@ is the type
of entity this is a key name for. -}
newtype Key a = Key { getKey :: String }

--------------------------------------------------------------------------------
{-| A basic type class for any entity that appears in the database, whether
it's core or not.

The minimal complete definition is 'tableName' and 'newFromRow'. -}
class HasTable a where
  {-| The name of this core entity's table in the PostgreSQL database.

  This has to wrapped in 'TableName' in order to make it polymorphic. -}
  tableName       :: TableName a

  {-| Create a new entity from the row in the database. 'coreEntityFromRow'
  will make use of this function, in order to create a 'LoadedCoreEntity'. -}
  newFromRow      :: Row -> a

--------------------------------------------------------------------------------
{-| A type class specifying that some data is currently stored in database,
and may have a primary key extracted. -}
class InDatabase a where
  {-| Extract the primary key for this value. -}
  rowKey :: a -> SqlValue

instance InDatabase (LoadedCoreEntity a) where
  rowKey = toSql . coreEntityVersion

instance InDatabase e => InDatabase (LoadedEntity e) where
  rowKey = rowKey . copoint

instance InDatabase (Ref a) where
  rowKey = rkey

--------------------------------------------------------------------------------
{-| A "core" entity is an entity that has both a GID (BookBrainz identifier)
and is also versioned. This type class defines how they can be interacted with
via the database. -}
class HasTable a => CoreEntity a where
  -- | Get a core entity by its GID.
  getByGid :: HasDatabase m
           => UUID
           -- ^ The GID of the core entity.
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
  getVersion :: (HasDatabase m, CoreEntity a)
             => Ref a
             -- ^ A reference to the version of the core entity.
             -> m (LoadedCoreEntity a)
  getVersion version = do
    results <- query selectQuery [ rowKey version ]
    return . coreEntityFromRow $ head results
    where table = getTableName (tableName :: TableName a)
          selectQuery = unlines  [ "SELECT *"
                                 , "FROM " ++ table
                                 , "WHERE version = ?" ]

  {-| Turn a 'Row' into a full 'LoadedCoreEntity'.

  You should use this with care, as it is possible to get a runtime exception here.
  This could happen if the 'Row' map doesn't contain sufficient columns to create
  the entity. -}
  coreEntityFromRow :: CoreEntity a
                    => Row
                    -- ^ The 'Row' - from the result of a SELECT.
                    -> LoadedCoreEntity a
  coreEntityFromRow row =
    CoreEntity { gid               = row ! "gid"
               , coreEntityVersion = row ! "version"
               , coreEntityInfo    = newFromRow row }

--------------------------------------------------------------------------------
{-| An entity is anything which is stored in the database, but is not a core
entity. -}
class HasTable a => Entity a where
  {-| The name of the primary key for this entity. By default this is @id@, but
  some tables (for example, country and language) may use other column names. -}
  key :: Key a
  key = Key "id"

  {-| Get this entity by its primary key. -}
  getByKey :: (HasDatabase m, Entity a, InDatabase r)
           => r
           -- ^ Some sort of reference to the entity to be fetched. Commonly,
           -- this will be a 'Ref'.
           -> m (LoadedEntity a)
  getByKey ref = do
    results <- query selectQuery [ rowKey ref ]
    return . entityFromRow $ head results
    where table = getTableName (tableName :: TableName a)
          key' = getKey (key :: Key a)
          selectQuery = unlines  [ "SELECT *"
                                 , "FROM " ++ table
                                 , "WHERE " ++ key' ++ " = ?" ]

  {-| Turn a 'Row' into a 'LoadedEntity'.

  You should use this with care, as it is possible to get a runtime exception here.
  This could happen if the 'Row' map doesn't contain sufficient columns to
  create the entity. -}
  entityFromRow :: Entity a
                => Row
                -- ^ The 'Row' - from the result of a SELECT.
                -> LoadedEntity a
  entityFromRow row = Entity $ newFromRow row
