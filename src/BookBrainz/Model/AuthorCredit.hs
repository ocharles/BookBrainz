module BookBrainz.Model.AuthorCredit
       ( getAuthorCredit
       ) where

import BookBrainz.Model
import qualified BookBrainz.Model.Person as P
import BookBrainz.Types
import Data.Map ((!))
import Database.HDBC (toSql, fromSql)

getAuthorCredit :: Ref AuthorCredit -> Model (LoadedEntity AuthorCredit)
getAuthorCredit acid = do
  credits <- query selectQuery [ toSql $ rid acid ]
  return Entity { entityId = rid acid
                , entityInfo = AuthorCredit { authorCredits = fromRows credits }
                }
  where selectQuery = unlines [ "SELECT *"
                              , "FROM author_credit_person"
                              , "JOIN person ON person.id = author_credit_person.person"
                              , "WHERE author_credit = ?"
                              , "ORDER BY position" ]
        fromRows = map fromRow
        fromRow row = Credit { creditedName = fromSql $ row ! "credited_name"
                             , creditedAuthor = P.fromRow row
                             , creditedJoinPhrase = fromSql $ row ! "join_phrase"
                             , creditedPosition = fromSql $ row ! "position"
                             }
