module BookBrainz.Model.Book
       ( getBook
       , findBookEditions
       , listAllBooks
       ) where

import BookBrainz.Model
import BookBrainz.Types
import Data.Map (Map, (!))
import Data.Maybe
import Data.UUID (UUID)
import Database.HDBC (SqlValue, toSql, fromSql)

bookFromRow :: Map String SqlValue -> LoadedCoreEntity Book
bookFromRow row = let book = Book { bookName         = fromSql $ row ! "name"
                                  , bookAuthorCredit = Ref $ fromSql $ row ! "author_credit"
                                  } in
                  CoreEntity { gid            = fromSql $ row ! "gid"
                             , coreEntityId   = fromSql $ row ! "id"
                             , coreEntityInfo = book }

listAllBooks :: Model [LoadedCoreEntity Book]
listAllBooks = map bookFromRow `fmap` query "SELECT * FROM book" [ ]

getBook :: UUID -> Model (Maybe (LoadedCoreEntity Book))
getBook bbid = do
  results <- query selectQuery [ toSql bbid ]
  return $ bookFromRow `fmap` listToMaybe results
  where selectQuery = unlines  [ "SELECT *"
                               , "FROM book"
                               , "WHERE gid = ?" ]

findBookEditions :: LoadedCoreEntity Book -> Model [LoadedCoreEntity Edition]
findBookEditions book = do
  results <- query selectQuery [ toSql $ rowKey book ]
  return $ fromRow `map` results
  where selectQuery = unlines [ "SELECT * "
                              , "FROM edition"
                              , "WHERE book = ?"
                              , "ORDER BY year, edition_index NULLS LAST"
                              ]
        fromRow row = CoreEntity { gid            = fromSql $ row ! "gid"
                                 , coreEntityInfo = editionFromRow row
                                 , coreEntityId   = fromSql $ row ! "id"
                                 }
        editionFromRow row = Edition { editionName        = fromSql $ row ! "name"
                                     , editionFormat      = maybeReference row "format"
                                     , editionBook        = toRef book
                                     , editionYear        = fromSql $ row ! "year"
                                     , editionPublisher   = maybeReference row "publisher"
                                     , editionCountry     = maybeReference row "country"
                                     , editionIllustrator = maybeReference row "illustrator"
                                     , editionTranslator  = maybeReference row "translator"
                                     , editionAuthor      = Ref $ fromSql $ row ! "author"
                                     , editionLanguage    = maybeReference row "language"
                                     , editionIsbn        = fromSql $ row ! "isbn"
                                     , editionBarcode     = fromSql $ row ! "barcode"
                                     , editionIndex       = fromSql $ row ! "edition_index"
                                     }
        maybeReference row column = Ref `fmap` fromSql (row ! column)
