-- | Functions for working with 'BookBrainz.Types.Book.Book' entities.
module BookBrainz.Model.Book
       ( -- * Working With Books
         listAllBooks
       , BookBrainz.Model.Book.create
       , addRole
       ) where

import Control.Monad                      (void)
import Data.Traversable                   (traverse)

import Data.Copointed                     (copoint)
import Database.HDBC                      (toSql, fromSql)

import BookBrainz.Model.Role              (copyRoles)
import BookBrainz.Search                  (indexBook)
import BrainzStem.Database                (queryOne, safeQueryOne
                                          ,HasDatabase, query)
import BrainzStem.Model as Model
import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))
import BookBrainz.Types                   (LoadedCoreEntity (..), Person
                                          ,Book(..), Role, Editor, Ref
                                          ,revisionTree, Concept
                                          ,Tree, Revision, LoadedEntity, Branch
                                          ,branchRevision, entityRef)

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

  newTree baseTree pubData = do
    versionId <- findOrInsertVersion
    newTreeId <- fromSql `fmap` queryOne insertTreeSql [ versionId ]
    traverse (\tree -> copyRoles tree newTreeId) baseTree
    return newTreeId
    where
      findOrInsertVersion = do
        foundId <- findVersion
        case foundId of
          Just id' -> return id'
          Nothing -> newVersion
      insertTreeSql = unlines [ "INSERT INTO bookbrainz_v.book_tree"
                              , "(version) VALUES (?)"
                              , "RETURNING book_tree_id"
                              ]
      findVersion =
        let findSql = unlines [ "SELECT version"
                              , "FROM bookbrainz_v.book_v"
                              , "WHERE name = ?"
                              ]
        in safeQueryOne findSql [ toSql $ bookName pubData ]
      newVersion =
        let insertSql = unlines [ "INSERT INTO bookbrainz_v.book_v"
                                , "(name) VALUES (?)"
                                , "RETURNING version"
                                ]
        in queryOne insertSql [ toSql $ bookName pubData ]

--------------------------------------------------------------------------------
-- | List the latest version of all known books.
listAllBooks :: (Functor a, HasDatabase a)
             => a [LoadedCoreEntity Book]
listAllBooks = map fromViewRow `fmap` query "SELECT * FROM book" [ ]

--------------------------------------------------------------------------------
-- | Create a new a book.
create :: (HasDatabase m)
       => Book                       {-^ The information about this book.. -}
       -> Ref Editor                 {-^ The editor creating this book. -}
       -> m (LoadedCoreEntity Book)  {-^ The book, loaded from the database
                                      (complete with BBID). -}
create bookData editor = do
  book <- Model.create bookData editor
  indexBook book []
  return book

--------------------------------------------------------------------------------
-- | Add a role to a 'Book'. This will create a new commit
addRole :: HasDatabase m
        => LoadedEntity (Branch Book)
        -> LoadedCoreEntity Book
        -> (Ref (Concept Person), Ref Role)
        -> Ref Editor
        -> m ()
addRole branch book (person, role) editor = do
  currentRev <- Model.getRevision $ branchRevision (copoint branch)
  newRev <- Model.newSystemRevision (Just $ revisionTree $ copoint currentRev)
                                    (copoint book)
                                    editor
  Model.parentRevision (entityRef newRev) (coreEntityRevision book)
  query addRoleSql [ toSql person
                   , toSql role
                   , toSql $ revisionTree $ copoint newRev
                   ]
  void $ Model.resetBranch (entityRef branch) (entityRef newRev)
  where addRoleSql = unlines [ "INSERT INTO bookbrainz.book_person_role"
                             , "(role_id, person_id, book_tree_id)"
                             , "VALUES (?, ?, ?)"
                             ]
