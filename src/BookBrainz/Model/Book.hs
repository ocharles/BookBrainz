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

bookFromRow :: Map String SqlValue -> Book
bookFromRow row = Book { bookId           = fromSql $ row ! "id"
                       , bookName         = fromSql $ row ! "name"
                       , bookGid          = fromSql $ row ! "gid"
                       , bookAuthorCredit = Ref $ fromSql $ row ! "author_credit"
                       }

listAllBooks :: Model [Book]
listAllBooks = map bookFromRow `fmap` query "SELECT * FROM book" [ ]

getBook :: UUID -> Model (Maybe Book)
getBook gid = do
  results <- query selectQuery [ toSql gid ]
  return $ bookFromRow `fmap` listToMaybe results
  where selectQuery = unlines  [ "SELECT *"
                               , "FROM book"
                               , "WHERE gid = ?" ]

findBookEditions :: Book -> Model [Edition]
findBookEditions book = do
  results <- query selectQuery [ toSql $ bookId book ]
  return $ fromRow `map` results
  where selectQuery = unlines [ "SELECT * "
                              , "FROM edition"
                              , "WHERE book = ?"
                              , "ORDER BY year, edition_index NULLS LAST"
                              ]
        fromRow row = Edition { editionId          = fromSql $ row ! "id"
                              , editionGid         = fromSql $ row ! "gid"
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
