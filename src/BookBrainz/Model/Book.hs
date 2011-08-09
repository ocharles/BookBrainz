-- | Functions for working with 'BookBrainz.Types.Book.Book' entities.
module BookBrainz.Model.Book
       ( -- * Working With Books
         getBook
       , listAllBooks
       , insertBook
       ) where

import Data.Maybe

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map               (Map, (!))
import Data.UUID              (UUID)
import Database.HDBC          (SqlValue, toSql, fromSql)
import System.Random

import BookBrainz.Database    (HasDatabase, query)
import BookBrainz.Types

--------------------------------------------------------------------------------
-- | Insert and version a new 'Book'.
insertBook :: (Functor m, HasDatabase m)
           => Book                       {-^ The information about the book to
                                             insert. -}
           -> m (LoadedCoreEntity Book)  {-^ The book, loaded from the database
                                             (complete with GID). -}
insertBook bookSpec = do
  bookGid <- liftIO randomIO :: MonadIO m => m UUID
  bookRow <- head `fmap` query insertQuery [ toSql $ bookName bookSpec
                                           , toSql   bookGid
                                           ]
  return $ bookFromRow bookRow
  where insertQuery = unlines [ "INSERT INTO bookbrainz_v.book (name, gid)"
                              , "VALUES (?, ?)"
                              , "RETURNING *"
                              ]

--------------------------------------------------------------------------------
{-| Create a 'Book' value, from a row in the database. The book is wrapped in
'LoadedCoreEntity' context, and will be complete with GID. -}
bookFromRow :: Map String SqlValue    -- ^ A 'Map' of attribute names to values.
            -> LoadedCoreEntity Book
bookFromRow row = let book = Book { bookName         = fromSql $ row ! "name"
                                  } in
                  CoreEntity { gid               = fromSql $ row ! "gid"
                             , coreEntityVersion = fromSql $ row ! "version"
                             , coreEntityInfo    = book }

--------------------------------------------------------------------------------
-- | List the latest version of all known books.
listAllBooks :: (Functor a, HasDatabase a) => a [LoadedCoreEntity Book]
listAllBooks = map bookFromRow `fmap` query "SELECT * FROM book" [ ]

--------------------------------------------------------------------------------
-- | Get a single book by GID.
getBook :: HasDatabase m
        => UUID                               {-^ The GID of the book to load. -}
        -> m (Maybe (LoadedCoreEntity Book))  {-^ The loaded book, or 'Nothing'
                                                  if the book could not be
                                                  found. -}
getBook bbid = do
  results <- query selectQuery [ toSql bbid ]
  return $ bookFromRow `fmap` listToMaybe results
  where selectQuery = unlines  [ "SELECT *"
                               , "FROM book"
                               , "WHERE gid = ?" ]
