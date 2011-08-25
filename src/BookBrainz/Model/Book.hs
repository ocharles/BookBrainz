-- | Functions for working with 'BookBrainz.Types.Book.Book' entities.
module BookBrainz.Model.Book
       ( -- * Working With Books
         listAllBooks
       , insertBook
       ) where

import Data.Maybe              (fromJust)

import Control.Monad.IO.Class  (liftIO, MonadIO)
import Data.UUID               (UUID)
import Database.HDBC           (toSql)
import System.Random           (randomIO)

import BrainzStem.Database     (HasDatabase, query, queryOne)
import BookBrainz.Model        (CoreEntity(..), HasTable(..), coreEntityFromRow
                               ,TableName(..), (!))
import BookBrainz.Types

instance HasTable Book where
  tableName = TableName "book"
  newFromRow row = Book { bookName = row ! "name"
                        }

instance CoreEntity Book

--------------------------------------------------------------------------------
-- | Insert and version a new 'Book'.
insertBook :: (Functor m, HasDatabase m, MonadIO m)
           => Book                       {-^ The information about the book to
                                             insert. -}
           -> m (LoadedCoreEntity Book)  {-^ The book, loaded from the database
                                             (complete with GID). -}
insertBook bookSpec = do
  bookGid <- liftIO randomIO :: MonadIO m => m UUID
  bookRow <- head `fmap` query insertQuery [ toSql $ bookName bookSpec
                                           , toSql   bookGid
                                           ]
  let book = coreEntityFromRow bookRow
  revisionId <- fromJust `fmap` queryOne revisionQuery [] :: (HasDatabase m, Functor m) => m (Maybe Int)
  query bookRevQuery [ toSql   revisionId
                     , toSql $ coreEntityVersion book
                     ]
  query branchQuery [ toSql True
                    , toSql revisionId
                    , toSql bookGid
                    ]
  return book
  where insertQuery = unlines [ "INSERT INTO bookbrainz_v.book (name, gid)"
                              , "VALUES (?, ?)"
                              , "RETURNING *"
                              ]
        revisionQuery = unlines [ "INSERT INTO bookbrainz_v.revision"
                                , "DEFAULT VALUES"
                                , "RETURNING rev_id"
                                ]
        bookRevQuery = unlines [ "INSERT INTO bookbrainz_v.book_revision"
                               , "(rev_id, version)"
                               , "VALUES (?, ?)"
                               ]
        branchQuery = unlines [ "INSERT INTO bookbrainz_v.branch"
                              , "(master, rev_id, gid)"
                              , "VALUES (?, ?, ?)"
                              ]

--------------------------------------------------------------------------------
-- | List the latest version of all known books.
listAllBooks :: (Functor a, HasDatabase a)
             => a [LoadedCoreEntity Book]
listAllBooks = map coreEntityFromRow `fmap` query "SELECT * FROM book" [ ]
