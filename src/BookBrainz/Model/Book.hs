-- | Functions for working with 'BookBrainz.Types.Book.Book' entities.
module BookBrainz.Model.Book
       ( -- * Working With Books
         listAllBooks
       , BookBrainz.Model.Book.create
       ) where

import Control.Monad (void)
import Control.Applicative (Applicative)
import Data.Traversable                   (traverse)

import Database.HDBC                      (toSql, fromSql)
import Snap.Snaplet.Hdbc (HasHdbc, query, run)

import BookBrainz.Model.Role              (copyRoles)
import BookBrainz.Search                  (indexBook)
import BrainzStem.Model as Model
import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))
import BookBrainz.Types                   (LoadedCoreEntity (..), Book(..)
                                          ,Editor, Ref)
import BrainzStem.Database (queryOne, safeQueryOne)

findOrInsertVersion dat = do
  foundId <- findVersion
  case foundId of
    Just id' -> return id'
    Nothing -> newVersion
  where findVersion =
          let findSql = unlines [ "SELECT version"
                                , "FROM bookbrainz_v.book_v"
                                , "WHERE name = ?"
                                ]
          in safeQueryOne findSql [ toSql $ bookName dat ]
        newVersion =
          let insertSql = unlines [ "INSERT INTO bookbrainz_v.book_v"
                                  , "(name) VALUES (?)"
                                  , "RETURNING version"
                                  ]
          in queryOne insertSql [ toSql $ bookName dat ]

instance GenericallyVersioned Book where
  versioningConfig = VersionConfig { cfgView = "book"
                                   , cfgConcept = "book"
                                   , cfgIdCol = "book_id"
                                   , cfgTree = "book_tree"
                                   , cfgRevision = "book_revision"
                                   , cfgBranch = "book_branch"
                                   , cfgBbid = "book_bbid"
                                   }

  fromViewRow row =
    CoreEntity { bbid = row ! "bbid"
               , coreEntityRevision = row ! "rev_id"
               , coreEntityTree = row ! "book_tree_id"
               , coreEntityConcept = row ! "book_id"
               , coreEntityInfo = Book { bookName = row ! "name" }
               }

  updateTreeImpl pubData tree = void $ do
    versionId <- findOrInsertVersion pubData
    run "UPDATE bookbrainz_v.book_tree SET version = ? WHERE book_tree_id = ?"
        [ versionId, toSql tree ]

  newTreeImpl pubData = do
    versionId <- findOrInsertVersion pubData
    newTreeId <- fromSql `fmap` queryOne insertTreeSql [ versionId ]
    return newTreeId
    where
      insertTreeSql = unlines [ "INSERT INTO bookbrainz_v.book_tree"
                              , "(version) VALUES (?)"
                              , "RETURNING book_tree_id"
                              ]

--------------------------------------------------------------------------------
-- | List the latest version of all known books.
listAllBooks :: (Functor m, HasHdbc m c s)
             => m [LoadedCoreEntity Book]
listAllBooks = map fromViewRow `fmap` query "SELECT * FROM book" [ ]

--------------------------------------------------------------------------------
-- | Create a new a book.
create :: (Functor m, HasHdbc m c s, Applicative m)
       => Book                       {-^ The information about this book.. -}
       -> Ref Editor                 {-^ The editor creating this book. -}
       -> m (LoadedCoreEntity Book)  {-^ The book, loaded from the database
                                      (complete with BBID). -}
create bookData editor = do
  book <- Model.create bookData editor
  indexBook book []
  return book
