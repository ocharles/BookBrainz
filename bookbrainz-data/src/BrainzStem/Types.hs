{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types overlooking the whole BrainzStem architecture.
module BrainzStem.Types
       ( LoadedCoreEntity (..)
       , LoadedEntity (..)
       , Ref
       , Revision (..)
       , Tree
       , Editor (..)
       , BBID
       , parseBbid
       , (:.)(..)
       ) where

import Data.Copointed
import Data.Text      (Text)
import Data.Time      (UTCTime)
import Data.Typeable  (Typeable)
import Data.UUID      (UUID, fromString)
import Database.PostgreSQL.Simple ((:.)(..))
import System.Random  (Random)

--------------------------------------------------------------------------------
-- | Represents a reference in a database. @entity@ is a phantom type which
-- tracks what type of entity this reference refers to.
data family Ref entity

--------------------------------------------------------------------------------
{-| A wrapper type that indicates that some data is a core BrainzStem entity,
which means it is both versioned, and has a BrainzStem identifier.
'LoadedCoreEntity' can also be thought of as some data in the core entity
context. As we have the ability to access the data within this context, this is
an instance of 'Copointed'. To work directly with the underlying data, use the
'copoint' function from 'Data.Copointed'. -}
data LoadedCoreEntity a = CoreEntity
    { -- | The BrainzStem identifier of this entity.
      bbid :: BBID a
      -- | The revision tracking this data.
    , coreEntityRevision :: Ref (Revision a)
      -- | A reference to this entity's tree.
    , coreEntityTree :: Ref (Tree a)
      -- | The underlying information about this entity.
    , coreEntityInfo :: a
    }

deriving instance ( Eq a, Eq (Ref (Tree a)), Eq (Ref (Revision a)))
  => Eq (LoadedCoreEntity a)

deriving instance ( Show a, Show (Ref (Tree a)), Show (Ref (Revision a)))
  => Show (LoadedCoreEntity a)

instance Copointed LoadedCoreEntity where
  copoint = coreEntityInfo

--------------------------------------------------------------------------------
{-| Represents other data that has been loaded from the database, but is not
a full core entity. -}
data LoadedEntity a = Entity
    { -- | The underlying information about this entity.
      entityInfo :: a
      -- | A reference to this entity in the database.
    , entityRef :: Ref a
    }

deriving instance (Eq a, Eq (Ref a))
  => Eq (LoadedEntity a)

deriving instance (Show a, Show (Ref a))
  => Show (LoadedEntity a)

instance Copointed LoadedEntity where
  copoint = entityInfo

--------------------------------------------------------------------------------
-- | Represents a single revision of an entity of type @a@.
data Revision a = Revision { -- | The 'Tree' this revision refers to.
                             revisionTree :: Ref (Tree a)
                           , revisionTime :: UTCTime
                           , revisionEditor :: Ref Editor
                           }

--------------------------------------------------------------------------------
{-| A tree is a collection of data about an entity, at a point in time (at a
revision, specifically). For an edition, this is the edition name, language, and
other properties, but also the people who have a role in the creation of the
edition, or a link to the publisher.

A 'Tree' itself has no value however, it is intended to be used with 'Ref', and
functions that operate on 'Tree' 'Ref's. -}
data Tree a

--------------------------------------------------------------------------------
{-| An editon within the BrainzStem system. -}
data Editor = Editor { -- | The name of the editor.
                       editorName :: Text
                     }

--------------------------------------------------------------------------------
-- | A BookBrainz identifier. @a@ is a phantom type, which stops you using this
-- BBID to refer to different entities.
newtype BBID a = BBID UUID
                 deriving (Eq, Typeable, Random)

instance Show (BBID a) where
  show (BBID uuid) = show uuid

--------------------------------------------------------------------------------
-- | Try and parse a 'BBID' from a 'String', returning 'Nothing' if the parse
-- fails.
parseBbid :: String -> Maybe (BBID a)
parseBbid = fmap BBID . fromString
