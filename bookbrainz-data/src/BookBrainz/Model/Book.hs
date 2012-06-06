{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for working with 'BookBrainz.Types.Book.Book' entities.
module BookBrainz.Model.Book
       ( -- * Working With Books
         listAllBooks
       , BookBrainz.Model.Book.create
       ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad (void)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Only(..))

import Snap.Snaplet.PostgresqlSimple (HasPostgres, execute, query_)

import BookBrainz.Model.Role              (copyRoles, HasRoles(..), copyRoles', addRole', findRoles')
import BrainzStem.Model as Model
import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))
import BookBrainz.Types                   (LoadedCoreEntity (..), Book(..)
                                          ,Editor, Ref(..))
import BrainzStem.Database (queryOne, safeQueryOne)

findOrInsertVersion dat = do
  foundId <- findVersion
  case foundId of
    Just id' -> return id'
    Nothing -> newVersion
  where findVersion =
          let findSql = fromString $ unlines [ "SELECT version"
                                , "FROM bookbrainz_v.book_v"
                                , "WHERE name = ?"
                                ]
          in (fmap fromOnly) <$> safeQueryOne findSql (Only $ bookName dat)
        newVersion =
          let insertSql = fromString $ unlines [ "INSERT INTO bookbrainz_v.book_v"
                                  , "(name) VALUES (?)"
                                  , "RETURNING version"
                                  ]
          in (\x -> x `asTypeOf` (undefined :: Int)) <$>
               (fmap fromOnly $ queryOne insertSql (Only $ bookName dat))

instance GenericallyVersioned Book where
  versioningConfig = VersionConfig { cfgView = "book"
                                   , cfgConcept = "book"
                                   , cfgIdCol = "book_id"
                                   , cfgTree = "book_tree"
                                   , cfgRevision = "book_revision"
                                   , cfgBranch = "book_branch"
                                   , cfgBbid = "book_bbid"
                                   }

  updateTreeImpl pubData tree = void $ do
    versionId <- findOrInsertVersion pubData
    execute "UPDATE bookbrainz_v.book_tree SET version = ? WHERE book_tree_id = ?"
      (versionId, tree)

  newTreeImpl pubData = do
    versionId <- findOrInsertVersion pubData
    newTreeId <- fromOnly `fmap` queryOne insertTreeSql (Only versionId)
    return newTreeId
    where
      insertTreeSql = fromString $ unlines [ "INSERT INTO bookbrainz_v.book_tree"
                              , "(version) VALUES (?)"
                              , "RETURNING book_tree_id"
                              ]

  cloneTreeImpl treeId = do
    newTreeId <- fromOnly `fmap` queryOne insertTreeSql (Only treeId)
    copyRoles treeId newTreeId
    return newTreeId
    where
      insertTreeSql = fromString $ unlines [ "INSERT INTO bookbrainz_v.book_tree"
                              , "(version) SELECT version FROM bookbrainz_v.book_tree"
                              , " WHERE book_tree_id = ?"
                              , "RETURNING book_tree_id"
                              ]

--------------------------------------------------------------------------------
-- | List the latest version of all known books.
listAllBooks :: (Functor m, HasPostgres m)
             => m [LoadedCoreEntity Book]
listAllBooks = query_ "SELECT * FROM book"

--------------------------------------------------------------------------------
-- | Create a new a book.
create :: (Functor m, HasPostgres m, Applicative m)
       => Book                       {-^ The information about this book.. -}
       -> Ref Editor                 {-^ The editor creating this book. -}
       -> m (LoadedCoreEntity Book)  {-^ The book, loaded from the database
                                      (complete with BBID). -}
create bookData editor = do
  book <- Model.create bookData editor
  return book

instance HasRoles Book where
  findRoles = findRoles' "book"
  copyRoles = copyRoles' "book"
  addRole = addRole' "book"
