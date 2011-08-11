-- | Functions for working with 'BookBrainz.Types.Book.Book' entities.
module BookBrainz.Model.Book
       ( -- * Working With Books
         listAllBooks
       , insertBook
       ) where

import Control.Monad.IO.Class  (liftIO, MonadIO)
import Data.Map                ((!))
import Data.UUID               (UUID)
import Database.HDBC           (fromSql, toSql)
import System.Random           (randomIO)

import BookBrainz.Database     (HasDatabase, query)
import BookBrainz.Model        (CoreEntity(..), coreEntityFromRow, TableName(..))
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
  return $ coreEntityFromRow bookRow
  where insertQuery = unlines [ "INSERT INTO bookbrainz_v.book (name, gid)"
                              , "VALUES (?, ?)"
                              , "RETURNING *"
                              ]

instance CoreEntity Book where
  tableName = TableName "book"
  newFromRow row = Book { bookName = fromSql $ row ! "name"
                        }

--------------------------------------------------------------------------------
-- | List the latest version of all known books.
listAllBooks :: (Functor a, HasDatabase a) => a [LoadedCoreEntity Book]
listAllBooks = map coreEntityFromRow `fmap` query "SELECT * FROM book" [ ]
