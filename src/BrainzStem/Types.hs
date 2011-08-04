module BrainzStem.Types
       ( InDatabase(..)
       , LoadedCoreEntity(..)
       , LoadedEntity(..)
       , Ref(..)
       ) where

import Data.Copointed
import Data.UUID
import Data.Convertible
import Database.HDBC (SqlValue, toSql)

-- |Represents a reference in a database. @entity@ is a phantom type which
-- tracks what type of entity this reference refers to.
data Ref entity = Ref { rid :: Int }
                deriving Show

instance Convertible (Ref a) SqlValue where
  safeConvert = Right . toSql . rid

class InDatabase entity where
  rowKey :: entity -> Int
  toRef  :: entity -> Ref entity

data LoadedCoreEntity a = CoreEntity { gid               :: UUID
                                     , coreEntityVersion :: Int
                                     , coreEntityInfo    :: a
                                     }
                        deriving Show

instance Copointed LoadedCoreEntity where
  copoint = coreEntityInfo

instance InDatabase (LoadedCoreEntity a) where
  rowKey = coreEntityVersion
  toRef entity = Ref { rid = rowKey entity }

data LoadedEntity a = Entity { entityId   :: Int
                             , entityInfo :: a
                             }
                    deriving Show

instance Copointed LoadedEntity where
  copoint = entityInfo

instance InDatabase (LoadedEntity a) where
  rowKey = entityId
  toRef entity = Ref { rid = rowKey entity }
