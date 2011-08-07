module BookBrainz.Model.Book
       ( getBook
       , findBookEditions
       , insertBook
       , listAllBooks
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map, (!))
import Data.Maybe
import System.Random
import Data.UUID (UUID)
import Database.HDBC (SqlValue, toSql, fromSql)

import BookBrainz.Database (HasDatabase, query)
import BookBrainz.Types

insertBook :: (Functor m, HasDatabase m) => Book -> m (LoadedCoreEntity Book)
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

bookFromRow :: Map String SqlValue -> LoadedCoreEntity Book
bookFromRow row = let book = Book { bookName         = fromSql $ row ! "name"
                                  } in
                  CoreEntity { gid               = fromSql $ row ! "gid"
                             , coreEntityVersion = fromSql $ row ! "version"
                             , coreEntityInfo    = book }

listAllBooks :: (Functor a, HasDatabase a) => a [LoadedCoreEntity Book]
listAllBooks = map bookFromRow `fmap` query "SELECT * FROM book" [ ]

getBook :: HasDatabase m => UUID -> m (Maybe (LoadedCoreEntity Book))
getBook bbid = do
  results <- query selectQuery [ toSql bbid ]
  return $ bookFromRow `fmap` listToMaybe results
  where selectQuery = unlines  [ "SELECT *"
                               , "FROM book"
                               , "WHERE gid = ?" ]

findBookEditions :: HasDatabase m => LoadedCoreEntity Book -> m [LoadedCoreEntity Edition]
findBookEditions book = do
  results <- query selectQuery [ toSql $ rowKey book ]
  return $ fromRow `map` results
  where selectQuery = unlines [ "SELECT * "
                              , "FROM edition"
                              , "WHERE book = ?"
                              , "ORDER BY year, edition_index NULLS LAST"
                              ]
        fromRow row = CoreEntity { gid               = fromSql $ row ! "gid"
                                 , coreEntityInfo    = editionFromRow row
                                 , coreEntityVersion = fromSql $ row ! "version"
                                 }
        editionFromRow row = Edition { editionName        = fromSql $ row ! "name"
                                     , editionFormat      = maybeReference row "format"
                                     , editionBook        = toRef book
                                     , editionYear        = fromSql $ row ! "year"
                                     , editionPublisher   = maybeReference row "publisher"
                                     , editionCountry     = maybeReference row "country"
                                     , editionLanguage    = maybeReference row "language"
                                     , editionIsbn        = fromSql $ row ! "isbn"
                                     , editionBarcode     = fromSql $ row ! "barcode"
                                     , editionIndex       = fromSql $ row ! "edition_index"
                                     }
        maybeReference row column = Ref `fmap` fromSql (row ! column)
