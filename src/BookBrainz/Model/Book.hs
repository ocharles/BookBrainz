-- | Functions for working with 'BookBrainz.Types.Book.Book' entities.
module BookBrainz.Model.Book
       ( -- * Working With Books
         listAllBooks
       ) where

import Data.Traversable                   (traverse)

import Database.HDBC                      (toSql, fromSql)

import BookBrainz.Model.Role              (copyRoles)
import BookBrainz.Types                   (Book (..))
import BrainzStem.Database                (queryOne, safeQueryOne, (!)
                                          ,HasDatabase, query)
import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))
import BrainzStem.Types                   (LoadedCoreEntity (..))

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
               , coreEntityRevision = row ! "revision"
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
