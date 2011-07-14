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

bookFromRow :: Map String SqlValue -> WithGid Book
bookFromRow row = let book = Book { bookId           = fromSql $ row ! "id"
                                  , bookName         = fromSql $ row ! "name"
                                  , bookAuthorCredit = Ref $ fromSql $ row ! "author_credit"
                                  } in
                  WithGid { gid = fromSql $ row ! "gid"
                          , info = book }

listAllBooks :: Model [WithGid Book]
listAllBooks = map bookFromRow `fmap` query "SELECT * FROM book" [ ]

getBook :: UUID -> Model (Maybe (WithGid Book))
getBook bbid = do
  results <- query selectQuery [ toSql bbid ]
  return $ bookFromRow `fmap` listToMaybe results
  where selectQuery = unlines  [ "SELECT *"
                               , "FROM book"
                               , "WHERE gid = ?" ]

findBookEditions :: Book -> Model [WithGid Edition]
findBookEditions book = do
  results <- query selectQuery [ toSql $ bookId book ]
  return $ fromRow `map` results
  where selectQuery = unlines [ "SELECT * "
                              , "FROM edition"
                              , "WHERE book = ?"
                              , "ORDER BY year, edition_index NULLS LAST"
                              ]
        fromRow row = WithGid { gid  = fromSql $ row ! "gid"
                              , info = editionFromRow row
                              }
        editionFromRow row = Edition { editionId          = fromSql $ row ! "id"
                                     , editionName        = fromSql $ row ! "name"
                                     , editionFormat      = maybeReference row "format"
                                     , editionBook        = book
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
