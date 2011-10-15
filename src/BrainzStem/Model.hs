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

         -- * Helper Functions
       , (!)

       , newSystemRevision
       , parentRevision
       , resetBranch
       ) where

import Control.Monad          (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Copointed         (copoint)
import Database.HDBC          (toSql, fromSql)
import System.Random          (randomIO)

import BrainzStem.Database    (HasDatabase, (!), queryOne, query)
import BrainzStem.Types       (LoadedCoreEntity (..), LoadedEntity (..)
                              ,Ref (..), Branch (..), Concept
                              ,Editor, Revision (..), Tree, BBID)

--------------------------------------------------------------------------------
{-| A "core" entity is an entity that has both a BBID (BookBrainz identifier)
and is also versioned. This type class defines how they can be interacted with
via the database. -}
class CoreEntity a where
  -- | Get a core entity by its BBID.
  getByBbid :: HasDatabase m
            => BBID a
            -- ^ The BBID of the core entity.
            -> m (Maybe (LoadedCoreEntity a))
            -- ^ The 'LoadedCoreEntity' contextual representation of this core
            -- entity, or 'Nothing' if there was no entity with this BBID. -}

  -- | Get the latest definition of an entity by its concept ID.
  -- This takes the tip of the master branch. You have to use a 'Ref' here
  -- because it's impossible to get a version of an entity without already
  -- knowing it's in the database.
  getByConcept :: (HasDatabase m, CoreEntity a)
               => Ref (Concept a)
               -- ^ A reference to the version of the core entity.
               -> m (LoadedCoreEntity a)

  -- | Create a new revision. If an existing revision is passed, copy all
  -- data from that tree and update that. Otherwise, a fresh tree should be
  -- created.
  newRevision :: HasDatabase m
              => Maybe (Ref (Tree a))
              -> a
              -> Ref (Revision a)
              -> m (LoadedEntity (Revision a))

  -- | Create a completely new concept and attach a BBID to it.
  newConcept :: HasDatabase m
             => BBID a
             -> m (Ref (Concept a))

  -- | Create a core entity specific branch and attach a concept reference to it
  attachBranchToConcept :: HasDatabase m
                        => Ref (Branch a)
                        -- ^ The reference the created branch should have.
                        -> Ref (Concept a)
                        -- ^ The concept the branch should reference.
                        -> m ()

  -- | Get a 'Revision' specific to this type of core entity.
  getRevision :: HasDatabase m
              => Ref (Revision a)
              -> m (LoadedEntity (Revision a))

  -- | Find the master 'Branch' for a specific concept.
  findMasterBranch :: HasDatabase m
                   => Ref (Concept a)
                   -> m (LoadedEntity (Branch a))

--------------------------------------------------------------------------------
-- | Insert and version a new core entity, creating a master branch and concept
-- at the same time. The creation of a concept will also assign a BBID to this
-- entity.
create :: (HasDatabase m, CoreEntity a)
       => a                       {-^ The information about the entity to
                                      insert. -}
       -> Ref Editor              {-^ The editor creating this core entity. -}
       -> m (LoadedCoreEntity a)  {-^ The entity, loaded from the database
                                      (complete with BBID). -}
create dat editorRef = do
  bbid' <- newSystemConcept
  concept <- newConcept bbid'
  revision <- newSystemRevision Nothing dat editorRef
  newSystemBranch concept (entityRef revision) True
  getByConcept concept

--------------------------------------------------------------------------------
-- | Create a new branch that points to the same revision as an existing branch.
-- This will usually be called with the master branch to create a new edit.
fork :: (CoreEntity a, HasDatabase m)
     => LoadedEntity (Branch a)
     -> m (Ref (Branch a))
fork branch =
  newSystemBranch (branchConcept $ copoint branch)
                  (branchRevision $ copoint branch)
                  False

--------------------------------------------------------------------------------
-- | Update a branch by creating a new revision at the tip of it, with the
-- parent set to the original tip of the branch.
update :: (CoreEntity a, HasDatabase m)
       => LoadedEntity (Branch a)
       -> a
       -> Ref Editor
       -> m ()
update branch dat editorRef = do
  let parent = branchRevision $ copoint branch
  currentRev <- getRevision $ branchRevision (copoint branch)
  newRev <- newSystemRevision (Just $ revisionTree $ copoint currentRev) dat editorRef
  parentRevision (entityRef newRev) parent
  resetBranch (entityRef branch) (entityRef newRev)

resetBranch :: (HasDatabase m, CoreEntity a)
            => Ref (Branch a)
            -> Ref (Revision a)
            -> m ()
resetBranch branch rev = do
  void $ query forwardSql [ toSql rev
                          , toSql branch
                          ]
  where forwardSql = unlines [ "UPDATE bookbrainz_v.branch SET rev_id = ?"
                             , "WHERE branch_id = ?"
                             ]

merge :: HasDatabase m
      => Ref (Concept a)
      -> Ref (Concept a)
      -> m a
merge = undefined

delete :: HasDatabase m
       => Ref (Concept a)
       -> m ()
delete = undefined

newSystemRevision :: (CoreEntity a, HasDatabase m)
                  => Maybe (Ref (Tree a))
                  -> a
                  -> Ref Editor
                  -> m (LoadedEntity (Revision a))
newSystemRevision base dat editor = do
  revId <- fromSql `fmap` queryOne revSql [ toSql editor ]
  newRevision base dat revId
  where revSql = unlines [ "INSERT INTO bookbrainz_v.revision"
                         , "(editor) VALUES (?)"
                         , "RETURNING rev_id"
                         ]

newSystemConcept :: HasDatabase m
                 => m (BBID a)
newSystemConcept = do
  uuid <- liftIO randomIO :: MonadIO m => m (BBID a)
  query bbidSql [ toSql uuid ]
  return uuid
  where bbidSql = "INSERT INTO bookbrainz_v.bbid (bbid) VALUES (?)"

newSystemBranch :: (CoreEntity a, HasDatabase m)
                => Ref (Concept a)
                -> Ref (Revision a)
                -> Bool
                -> m (Ref (Branch a))
newSystemBranch concept rev master = do
  branch <- fromSql `fmap` queryOne branchSql [ toSql rev
                                              , toSql master]
  attachBranchToConcept branch concept
  return branch
  where branchSql = unlines [ "INSERT INTO bookbrainz_v.branch"
                            , "(rev_id, master) VALUES (?, ?)"
                            , "RETURNING branch_id"
                            ]

parentRevision :: HasDatabase m
               => Ref (Revision a)
               -> Ref (Revision a)
               -> m ()
parentRevision child parent = do
  query parentSql [ toSql child
                  , toSql parent
                  ]
  return ()
  where parentSql = unlines [ "INSERT INTO bookbrainz_v.revision_parent"
                            , "(rev_id, parent_id) VALUES (?, ?)"
                            ]

--------------------------------------------------------------------------------
{-| An entity is anything which is stored in the database, but is not a core
entity. -}
class Entity a where
  {-| Get this entity by its primary key. -}
  getByPk :: (HasDatabase m, Entity a)
          => Ref a
          -- ^ Some sort of reference to the entity to be fetched.
          -> m (LoadedEntity a)
