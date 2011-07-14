module BookBrainz.Model.AuthorCredit
       ( getAuthorCredit
       , insertSimpleAuthorCredit
       ) where

import BookBrainz.Model
import BookBrainz.Model.Person (insertPerson)
import qualified BookBrainz.Model.Person as P
import BookBrainz.Types
import BrainzStem.Types
import Control.Applicative
import Database.HDBC (toSql, fromSql)
import Data.Map ((!))
import Data.Text (Text)

getAuthorCredit :: Ref (LoadedEntity AuthorCredit) -> Model (LoadedEntity AuthorCredit)
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
insertSimpleAuthorCredit :: Text -> Model (LoadedEntity AuthorCredit)
insertSimpleAuthorCredit name = do
  person <- insertPerson Person { personName = name }
  creditId <- fromSql . (! "id") . head <$> query insertAC [ ]
  creditRows <- query insertCredits [ toSql   creditId
                                    , toSql   name
                                    , toSql $ rid $ toRef person
                                    ]
  return Entity {
    entityId = creditId,
    entityInfo = AuthorCredit {
      authorCredits = creditFromRow person `map` creditRows
      }
    }
  where insertAC = unlines [ "INSERT INTO author_credit"
                           , "DEFAULT VALUES"
                           , "RETURNING id"
                           ]
        creditFromRow person row = Credit { creditedName = fromSql $ row ! "credited_name"
                                          , creditedAuthor = person
                                          , creditedJoinPhrase = fromSql $ row ! "join_phrase"
                                          ,creditedPosition = fromSql $ row ! "position"
                                          }
        insertCredits = unlines [ "INSERT INTO author_credit_person"
                                , "(author_credit, credited_name, person)"
                                , "VALUES"
                                , "(?, ?, ?)"
                                , "RETURNING *"
                                ]
