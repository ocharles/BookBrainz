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
       , create
       , update
         -- These are commented out until the actual definitions are defined
         --, delete
         --, merge
       , fork
       , Changes
       , forkRevision
       , applyChanges
       , revisionUnderChange
       , changeBranch

       , newSystemRevision
       , parentRevision
       , resetBranch
       ) where

import Control.Applicative (Applicative)
import Control.Monad          (void)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT, asks)
import Data.Copointed         (copoint)
import Data.String  (fromString)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Snap.Snaplet.PostgresqlSimple
import System.Random          (randomIO)

import BookBrainz.Schema ()
import BrainzStem.Database (queryOne)
import BrainzStem.Types       (LoadedCoreEntity (..), LoadedEntity (..)
                              ,Ref, Branch (..), Concept
                              ,Editor, Revision (..), Tree, BBID)

data ChangeEnvironment e =
    ChangeEnvironment { changeRevision :: LoadedEntity (Revision e)
                      , changePostgres :: Postgres
                      }

instance HasPostgres (Changes e) where
  getPostgresState = asks changePostgres

newtype Changes e a = Changes { changeActions :: ReaderT (ChangeEnvironment e) IO a }
        deriving ( Monad, MonadIO, MonadReader (ChangeEnvironment e)
                 , Applicative, Functor, MonadCatchIO)

forkRevision :: (Applicative m, CoreEntity a, HasPostgres m, FromField (Ref (Revision a)), ToField (Ref (Revision a)))
             => LoadedEntity (Revision a)
             -> Ref Editor
             -> Changes a ()
             -> m (LoadedEntity (Revision a))
forkRevision revision editor changes = do
  newRev <- newSystemRevision (revisionTree $ copoint revision) editor
  parentRevision (entityRef newRev) (entityRef revision)
  applyChanges changes newRev
  return newRev

applyChanges :: (HasPostgres m, Functor m)
             => Changes e a
             -> LoadedEntity (Revision e)
             -> m a
applyChanges changes revision = do
  pgState <- getPostgresState
  liftIO $ runReaderT (changeActions changes) (ChangeEnvironment revision pgState)

changeBranch :: (CoreEntity a, HasPostgres m, Applicative m, ToField (Ref (Branch a)), ToField (Ref (Revision a)), FromField (Ref (Revision a)))
             => LoadedEntity (Branch a)-> Ref Editor -> Changes a () -> m ()
changeBranch branch editor changes = do
  currentRev <- getRevision $ branchRevision $ copoint branch
  newRev <- forkRevision currentRev editor changes
  resetBranch (entityRef branch) (entityRef newRev)

revisionUnderChange :: forall a. Changes a (LoadedEntity (Revision a))
revisionUnderChange = asks changeRevision

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
            -- entity, or 'Nothing' if there was no entity with this BBID. -}

  -- | Get the latest definition of an entity by its concept ID.
  -- This takes the tip of the master branch. You have to use a 'Ref' here
  -- because it's impossible to get a version of an entity without already
  -- knowing it's in the database.
  getByConcept :: (HasPostgres m, CoreEntity a, Functor m)
               => Ref (Concept a)
               -- ^ A reference to the version of the core entity.
               -> m (LoadedCoreEntity a)

  -- | Create a new revision. If an existing revision is passed, copy all
  -- data from that tree and update that. Otherwise, a fresh tree should be
  -- created.
  newRevision :: (Applicative m, HasPostgres m, Functor m)
              => Ref (Tree a)
              -> Ref (Revision a)
              -> m (LoadedEntity (Revision a))

  -- | Create a completely new concept and attach a BBID to it.
  newConcept :: (HasPostgres m, Functor m)
             => BBID a
             -> m (Ref (Concept a))

  -- | Create a core entity specific branch and attach a concept reference to it
  attachBranchToConcept :: HasPostgres m
                        => Ref (Branch a)
                        -- ^ The reference the created branch should have.
                        -> Ref (Concept a)
                        -- ^ The concept the branch should reference.
                        -> m ()

  -- | Get a 'Revision' specific to this type of core entity.
  getRevision :: (Functor m, HasPostgres m)
              => Ref (Revision a)
              -> m (LoadedEntity (Revision a))

  -- | Find the master 'Branch' for a specific concept.
  findMasterBranch :: (HasPostgres m, Functor m)
                   => Ref (Concept a)
                   -> m (LoadedEntity (Branch a))

  getInfo :: (HasPostgres m) => Ref (Tree a) -> m a

  newTree :: (HasPostgres m, Functor m) => a -> m (Ref (Tree a))

  updateTree :: a -> (Ref (Tree a)) -> Changes a ()

--------------------------------------------------------------------------------
-- | Insert and version a new core entity, creating a master branch and concept
-- at the same time. The creation of a concept will also assign a BBID to this
-- entity.
create :: (HasPostgres m, CoreEntity a, Functor m, Applicative m, FromField (Ref (Revision a)), FromField (Ref (Branch a)), ToField (Ref (Revision a)))
       => a                       {-^ The information about the entity to
                                      insert. -}
       -> Ref Editor              {-^ The editor creating this core entity. -}
       -> m (LoadedCoreEntity a)  {-^ The entity, loaded from the database
                                      (complete with BBID). -}
create dat editorRef = do
  bbid' <- newSystemConcept
  concept <- newConcept bbid'
  tree <- newTree dat
  revision <- newSystemRevision tree editorRef
  newSystemBranch concept (entityRef revision) True
  getByConcept concept

--------------------------------------------------------------------------------
-- | Create a new branch that points to the same revision as an existing branch.
-- This will usually be called with the master branch to create a new edit.
fork :: (CoreEntity a, HasPostgres m, Functor m, FromField (Ref (Branch a)), ToField (Ref (Revision a)))
     => LoadedEntity (Branch a)
     -> m (Ref (Branch a))
fork branch =
  newSystemBranch (branchConcept $ copoint branch)
                  (branchRevision $ copoint branch)
                  False

--------------------------------------------------------------------------------
-- | Update a branch by creating a new revision at the tip of it, with the
-- parent set to the original tip of the branch.
update :: (CoreEntity a)
       => a
       -> Changes a ()
update dat = ((revisionTree . copoint) `fmap` revisionUnderChange)
               >>= updateTree dat

resetBranch :: (Functor m, HasPostgres m, CoreEntity a, ToField (Ref (Branch a)), ToField (Ref (Revision a)))
            => Ref (Branch a)
            -> Ref (Revision a)
            -> m ()
resetBranch branch rev = do
  void $ execute forwardSql (rev, branch)
  where forwardSql = fromString $ unlines [ "UPDATE bookbrainz_v.branch SET rev_id = ?"
                                          , "WHERE branch_id = ?"
                                          ]

merge :: HasPostgres m
      => Ref (Concept a)
      -> Ref (Concept a)
      -> m a
merge = undefined

delete :: HasPostgres m
       => Ref (Concept a)
       -> m ()
delete = undefined

-- XXX Bad name
newSystemRevision :: (CoreEntity a, HasPostgres m, Functor m, Applicative m, FromField (Ref (Revision a)))
                  => Ref (Tree a)
                  -> Ref Editor
                  -> m (LoadedEntity (Revision a))
newSystemRevision tree editor = do
  revId <- fromOnly `fmap` queryOne revSql (Only editor)
  newRevision tree revId
  where revSql = fromString $ unlines [ "INSERT INTO bookbrainz_v.revision"
                                      , "(editor) VALUES (?)"
                                      , "RETURNING rev_id"
                                      ]

newSystemConcept :: HasPostgres m
                 => m (BBID a)
newSystemConcept = do
  uuid <- liftIO randomIO :: MonadIO m => m (BBID a)
  execute bbidSql (Only uuid)
  return uuid
  where bbidSql = "INSERT INTO bookbrainz_v.bbid (bbid) VALUES (?)"

newSystemBranch :: (CoreEntity a, HasPostgres m, Functor m, FromField (Ref (Branch a)), ToField (Ref (Revision a)))
                => Ref (Concept a)
                -> Ref (Revision a)
                -> Bool
                -> m (Ref (Branch a))
newSystemBranch concept rev master = do
  branch <- fromOnly `fmap` queryOne branchSql (rev, master)
  attachBranchToConcept branch concept
  return branch
  where branchSql = fromString $ unlines [ "INSERT INTO bookbrainz_v.branch"
                                         , "(rev_id, master) VALUES (?, ?)"
                                         , "RETURNING branch_id"
                                         ]

parentRevision :: (HasPostgres m, ToField (Ref (Revision a)))
               => Ref (Revision a)
               -> Ref (Revision a)
               -> m ()
parentRevision child parent = do
  execute parentSql (child, parent)
  return ()
  where parentSql = fromString $ unlines [ "INSERT INTO bookbrainz_v.revision_parent"
                                         , "(rev_id, parent_id) VALUES (?, ?)"
                                         ]

--------------------------------------------------------------------------------
{-| An entity is anything which is stored in the database, but is not a core
entity. -}
class Entity a where
  {-| Get this entity by its primary key. -}
  getByPk :: (Functor m, HasPostgres m, Entity a)
          => Ref a
          -- ^ Some sort of reference to the entity to be fetched.
          -> m (LoadedEntity a)
