{-# LANGUAGE MultiParamTypeClasses #-}
-- | Types overlooking the whole BrainzStem architecture.
module BrainzStem.Types
       ( LoadedCoreEntity(..)
       , LoadedEntity(..)
       , Ref(..)
       ) where

import Data.Convertible (Convertible(..))
import Data.Copointed
import Data.UUID
import Database.HDBC    (SqlValue)

-- Import for type class instances
import BrainzStem.Types.Newtypes ()

--------------------------------------------------------------------------------
-- | Represents a reference in a database. @entity@ is a phantom type which
-- tracks what type of entity this reference refers to.
data Ref entity = Ref { rkey :: SqlValue }
                deriving Show

instance Convertible (Ref a) SqlValue where
  safeConvert = Right . rkey

instance Convertible SqlValue (Ref a) where
  safeConvert id' = Right Ref { rkey = id' }

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

--------------------------------------------------------------------------------
{-| Represents other data that has been loaded from the database, but is not
a full core entity. -}
data LoadedEntity a = Entity
    { -- | The underlying information about this entity.
      entityInfo :: a
    } deriving Show

instance Copointed LoadedEntity where
  copoint = entityInfo
