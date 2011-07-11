module BookBrainz.Model.Book
       ( getBook
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
