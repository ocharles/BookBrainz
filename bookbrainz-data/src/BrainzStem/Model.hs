{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-| Common functions for accessing entities. -}
module BrainzStem.Model
       (
         -- * Entity Manipulation
         CoreEntity(..)
       , Entity(..)
       ) where

import Control.Applicative (Applicative)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader(..), asks)
import Snap.Snaplet.PostgresqlSimple

import BookBrainz.Schema ()
import BrainzStem.Types       (LoadedCoreEntity (..), LoadedEntity (..)
                              ,Ref, Revision (..), BBID)

data ChangeEnvironment e =
    ChangeEnvironment { changeRevision :: LoadedEntity (Revision e)
                      , changePostgres :: Postgres
                      }

instance HasPostgres (Changes e) where
  getPostgresState = asks changePostgres

newtype Changes e a = Changes { changeActions :: ReaderT (ChangeEnvironment e) IO a }
        deriving ( Monad, MonadIO, MonadReader (ChangeEnvironment e)
                 , Applicative, Functor, MonadCatchIO)

--------------------------------------------------------------------------------
{-| A "core" entity is an entity that has both a BBID (BookBrainz identifier)
and is also versioned. This type class defines how they can be interacted with
via the database. -}
class CoreEntity a where
  -- | Get a core entity by its BBID.
  getByBbid :: (Functor m, HasPostgres m)
            => BBID a
            -- ^ The BBID of the core entity.
            -> m (Maybe (LoadedCoreEntity a))
            -- ^ The 'LoadedCoreEntity' contextual representation of this core
            -- entity, or 'Nothing' if there was no entity with this BBID.

--------------------------------------------------------------------------------
{-| An entity is anything which is stored in the database, but is not a core
entity. -}
class Entity a where
  {-| Get this entity by its primary key. -}
  getByPk :: (Functor m, HasPostgres m, Entity a)
          => Ref a
          -- ^ Some sort of reference to the entity to be fetched.
          -> m (LoadedEntity a)
