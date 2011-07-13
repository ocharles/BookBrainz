module BookBrainz.Model.Book
       ( getBook
       , loadAuthorCredit
       , findBookEditions
       , listAllBooks
       ) where

import BookBrainz.Model (query)
import BookBrainz.Types
import BookBrainz.Types.MVC (Model)
import BookBrainz.Types.Newtypes
import Data.Map (Map, (!))
import Data.Maybe
import Data.UUID (UUID)
import Database.HDBC (SqlValue, toSql, fromSql)

bookFromRow :: Map String SqlValue -> Book
bookFromRow row = Book { bookId           = BookId $ fromSql $ row ! "id"
                       , bookName         = fromSql $ row ! "name"
                       , bookGid          = fromSql $ row ! "gid"
                       , bookAuthorCredit = AuthorCreditReference $ AuthorCreditId $ fromSql $ row ! "author_credit"
                       }

listAllBooks :: Model [Book]
listAllBooks = (map bookFromRow) `fmap` query "SELECT * FROM book" [ ]

getBook :: UUID -> Model (Maybe Book)
getBook gid = do
  results <- query selectQuery [ toSql gid ]
  return $ (listToMaybe results) >>= (return . bookFromRow)
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
        fromRow row = Edition { editionId = fromSql $ row ! "id"
                              , editionGid = fromSql $ row ! "gid"
                              , editionName = fromSql $ row ! "name"
                              , editionFormat = maybeReference row "format" EditionFormatReference
                              , editionBook = book
                              , editionYear = fromSql $ row ! "year"
                              , editionPublisher = maybeReference row "publisher" PublisherReference
                              , editionCountry = maybeReference row "country" CountryReference
                              , editionIllustrator = maybeReference row "illustrator" PersonReference
                              , editionTranslator = maybeReference row "translator" PersonReference
                              , editionAuthor = AuthorCreditReference $ fromSql $ row ! "author"
                              , editionLanguage = maybeReference row "language" LanguageReference
                              , editionIsbn = fromSql $ row ! "isbn"
                              , editionBarcode = fromSql $ row ! "barcode"
                              , editionIndex = fromSql $ row ! "edition_index"
                              }
        maybeReference row column constructor = (fromSql $ row ! column) >>= (return . constructor)

loadAuthorCredit :: Book -> Model Book
loadAuthorCredit book = case bookAuthorCredit book of
  AuthorCreditReference orig@(AuthorCreditId acid) -> do
    credits <- query selectQuery [ toSql acid ]
    return book { bookAuthorCredit = AuthorCredit { authorCreditId = orig
                                                  , authorCredits = fromRows credits }
                }
  _ -> return book
  where selectQuery = unlines [ "SELECT *"
                              , "FROM author_credit_person"
                              , "WHERE author_credit = ?"
                              , "ORDER BY position" ]
        fromRows = map fromRow
        fromRow row = Credit { creditedName = fromSql $ row ! "credited_name"
                             , creditedAuthor = PersonReference $ PersonId $ fromSql $ row ! "person"
                             , creditedJoinPhrase = fromSql $ row ! "join_phrase"
                             , creditedPosition = fromSql $ row ! "position"
                             }
