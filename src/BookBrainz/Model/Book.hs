-- | Functions for working with 'BookBrainz.Types.Book.Book' entities.
module BookBrainz.Model.Book
       ( -- * Working With Books
         listAllBooks
       , insertBook
       , updateBook
       ) where

import Control.Monad               (when)
import Data.Maybe                  (fromJust, isJust)

import Control.Monad.IO.Class      (liftIO, MonadIO)
import Data.Map                    (union)
import Data.UUID                   (UUID)
import Database.HDBC               (toSql)
import System.Random               (randomIO)

import BrainzStem.Database         (HasDatabase, query)
import BrainzStem.Model            (CoreEntity(..), HasTable(..)
                                   ,coreEntityFromRow, TableName(..), (!)
                                   , InDatabase (..))
import BookBrainz.Types
import BrainzStem.Model.Versioning ()

instance HasTable Book where
  tableName = TableName "book"
  newFromRow row = Book { bookName = row ! "name"
                        }

instance CoreEntity Book

--------------------------------------------------------------------------------
-- | Insert and version a new 'Book', creating a master branch at the same time.
insertBook :: (Functor m, HasDatabase m, MonadIO m)
           => Book                       {-^ The information about the book to
                                             insert. -}
           -> m (LoadedCoreEntity Book)  {-^ The book, loaded from the database
                                             (complete with GID). -}
insertBook bookSpec = do
  bookGid <- liftIO randomIO :: MonadIO m => m UUID
  book <- addVersion bookSpec bookGid Nothing
  addBranch book True
  return book

--------------------------------------------------------------------------------
{-| Update an existing book by creating a new version, and forking the existing
revision. If this is done in the context of an existing branch, that branch
will be updated, otherwise a new branch will be created. -}
updateBook :: (HasDatabase m, Functor m)
           => LoadedCoreEntity Book
           -> Book
           -> Maybe (LoadedEntity Branch)
           -> m (LoadedCoreEntity Book)
updateBook origBook bookSpec branchContext = do
  book <- addVersion bookSpec (gid origBook) (Just $ coreEntityRevision origBook)
  when (isJust branchContext) $
    query updateBranchQuery [ toSql $ coreEntityRevision book
                            , rowKey (fromJust branchContext)
                            ] >> return ()
  return book
  where updateBranchQuery = unlines [ "UPDATE bookbrainz_v.branch"
                                    , "SET rev_id = ?"
                                    , "WHERE id = ?" ]

--------------------------------------------------------------------------------
-- | List the latest version of all known books.
listAllBooks :: (Functor a, HasDatabase a)
             => a [LoadedCoreEntity Book]
listAllBooks = map coreEntityFromRow `fmap` query "SELECT * FROM book" [ ]

--------------------------------------------------------------------------------
-- | Creates a book version, with no branch. You will need to either create
-- a branch, or update an existing branch.
addVersion :: (HasDatabase m, Functor m)
           => Book
           -- ^ The data for this version.
           -> UUID
           -- ^ The UUID of this version.
           -> Maybe (Ref (LoadedEntity Revision))
           -- ^ The parent revision of this revision, or Nothing to create a
           -- new revision history.
           -> m (LoadedCoreEntity Book)
addVersion bookSpec uuid parent = do
  bookRow <- head `fmap` query insertQuery [ toSql $ bookName bookSpec
                                           , toSql   uuid
                                           ]
  revisionRow <- head `fmap` query revisionQuery [ ]
  let book = coreEntityFromRow (bookRow `union` revisionRow)
  query bookRevQuery [ toSql $ coreEntityRevision book
                     , toSql $ coreEntityVersion book
                     ]
  when (isJust parent) $
    query parentQuery [ toSql   parent
                      , toSql $ coreEntityRevision book
                      ] >> return ()
  return book
  where insertQuery = unlines [ "INSERT INTO bookbrainz_v.book (name, gid)"
                              , "VALUES (?, ?)"
                              , "RETURNING *"
                              ]
        revisionQuery = unlines [ "INSERT INTO bookbrainz_v.revision"
                                , "DEFAULT VALUES"
                                , "RETURNING rev_id AS revision"
                                ]
        parentQuery = unlines [ "INSERT INTO bookbrainz_v.revision_parent"
                              , "(parent_id, rev_id) VALUES (?, ?)"
                              ]
        bookRevQuery = unlines [ "INSERT INTO bookbrainz_v.book_revision"
                               , "(rev_id, version)"
                               , "VALUES (?, ?)"
                               ]

--------------------------------------------------------------------------------
-- | Create a new branch.
addBranch :: HasDatabase m
          => LoadedCoreEntity Book
          -- ^ The book revision to make the tip of the branch.
          -> Bool
          -- ^ Whether or not this branch should be considered the master
          -- branch.
          -> m ()
addBranch book asMaster =
  query branchQuery [ toSql   asMaster
                    , toSql $ coreEntityRevision book
                    , toSql $ gid book
                    ]
    >> return ()
  where branchQuery = unlines [ "INSERT INTO bookbrainz_v.branch"
                              , "(master, rev_id, gid)"
                              , "VALUES (?, ?, ?)"
                              ]
