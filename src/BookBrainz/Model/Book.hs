-- | Functions for working with 'BookBrainz.Types.Book.Book' entities.
module BookBrainz.Model.Book
       ( -- * Working With Books
         listAllBooks
       ) where

import Control.Monad          (when)
import Data.Maybe             (isJust)

import Data.Map               (union)
import Database.HDBC          (toSql)

import BrainzStem.Database    (HasDatabase, query)
import BrainzStem.Model       (CoreEntity(..), HasTable(..)
                              ,coreEntityFromRow, TableName(..), (!))
import BookBrainz.Types

instance HasTable Book where
  tableName = TableName "book"
  newFromRow row = Book { bookName = row ! "name"
                        }

instance CoreEntity Book where
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
-- | List the latest version of all known books.
listAllBooks :: (Functor a, HasDatabase a)
             => a [LoadedCoreEntity Book]
listAllBooks = map coreEntityFromRow `fmap` query "SELECT * FROM book" [ ]
