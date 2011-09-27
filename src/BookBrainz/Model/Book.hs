-- | Functions for working with 'BookBrainz.Types.Book.Book' entities.
module BookBrainz.Model.Book
       ( -- * Working With Books
         listAllBooks
       ) where

import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))

import Database.HDBC                      (toSql, fromSql)

import BookBrainz.Types                   (Book (..))
import BrainzStem.Database                (queryOne, safeQueryOne, (!)
                                          ,HasDatabase, query)
import BrainzStem.Types                   (LoadedCoreEntity (..))

instance GenericallyVersioned Book where
  versioningConfig = VersionConfig { cfgView = "book"
                                   , cfgIdCol = "book_id"
                                   }

  fromViewRow row =
    CoreEntity { bbid = row ! "bbid"
               , coreEntityRevision = row ! "revision"
               , coreEntityTree = row ! "book_tree_id"
               , coreEntityConcept = row ! "book_id"
               , coreEntityInfo = Book { bookName = row ! "name" }
               }

  findVersion pubData = fmap fromSql `fmap`
                          safeQueryOne findSql [ toSql $ bookName pubData ]
    where findSql = unlines [ "SELECT version"
                            , "FROM bookbrainz_v.book_v"
                            , "WHERE name = ?"
                            ]

  newVersion pubData = fromSql `fmap`
                         queryOne insertSql [ toSql $ bookName pubData ]
    where insertSql = unlines [ "INSERT INTO bookbrainz_v.book_v"
                              , "(name) VALUES (?)"
                              , "RETURNING version"
                              ]

--------------------------------------------------------------------------------
-- | List the latest version of all known books.
listAllBooks :: (Functor a, HasDatabase a)
             => a [LoadedCoreEntity Book]
listAllBooks = map fromViewRow `fmap` query "SELECT * FROM book" [ ]
