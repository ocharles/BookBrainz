-- | Types overlooking the whole BrainzStem architecture
module BrainzStem.Types
       ( InDatabase(..)
       , LoadedCoreEntity(..)
       , LoadedEntity(..)
       , Ref(..)
       ) where

import Data.Convertible
import Data.Copointed
import Data.UUID
import Database.HDBC (SqlValue, toSql)

--------------------------------------------------------------------------------
-- | Represents a reference in a database. @entity@ is a phantom type which
-- tracks what type of entity this reference refers to.
data Ref entity = Ref { rid :: Int }
                deriving Show

instance Convertible (Ref a) SqlValue where
  safeConvert = Right . toSql . rid

--------------------------------------------------------------------------------
{-| A typeclass specifying that some data is currently stored in database,
and may have a primary key extracted. -}
class InDatabase entity where
  -- | Retrieve the primary key for a given entity.
  rowKey :: entity -> Int
  -- | Transform an entity into a reference for other objects to point to.
  toRef  :: entity -> Ref entity

--------------------------------------------------------------------------------
{-| A wrapper type that indicates that some data is a core BrainzStem entity,
which means it is both versioned, and has a BrainzStem identifier.
'LoadedCoreEntity' can also be thought of as some data in the core entity
context. As we have the ability to access the data within this context, this is
an instance of 'Copointed'. To work directly with the underlying data, use the
'copoint' function from 'Data.Copointed'. -}
data LoadedCoreEntity a = CoreEntity
    { -- | The BrainzStem identifier of this entity.
      gid               :: UUID
      -- | The version of this data.
    , coreEntityVersion :: Int
      -- | The underlying information about this entity.
    , coreEntityInfo    :: a
    } deriving Show

instance Copointed LoadedCoreEntity where
  copoint = coreEntityInfo

instance InDatabase (LoadedCoreEntity a) where
  rowKey = coreEntityVersion
  toRef entity = Ref { rid = rowKey entity }

--------------------------------------------------------------------------------
{-| Represents other data that has been loaded from the database, but is not
a full core entity. -}
data LoadedEntity a = Entity
    { -- | The row ID of this entity.
      entityId   :: Int
      -- | The underlying information about this entity.
    , entityInfo :: a
    } deriving Show

instance Copointed LoadedEntity where
  copoint = entityInfo

instance InDatabase (LoadedEntity a) where
  rowKey = entityId
  toRef entity = Ref { rid = rowKey entity }
