module BookBrainz.Model.Book
       ( getBook
       , loadAuthorCredit
       ) where

import BookBrainz.Model (query)
import BookBrainz.Types
import BookBrainz.Types.MVC (Model)
import BookBrainz.Types.Newtypes
import Data.Map ((!))
import Data.Maybe
import Database.HDBC (toSql, fromSql)

getBook :: BookId -> Model (Maybe Book)
getBook (BookId bid) = do
  results <- query selectQuery [ toSql bid ]
  return $ (listToMaybe results) >>= fromRow
  where selectQuery = unlines  [ "SELECT *"
                               , "FROM book"
                               , "WHERE id = ?" ]
        fromRow row = Just $ Book { bookId = BookId $ fromSql $ row ! "id"
                                  , bookName = fromSql $ row ! "name"
                                  , bookGid = fromSql $ row ! "gid"
                                  , bookAuthorCredit = AuthorCreditReference $ AuthorCreditId $ fromSql $ row ! "author_credit"
                                  }
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
