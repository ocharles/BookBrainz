module BookBrainz.Model.Person
       ( loadForAuthorCredits
       ) where

import BookBrainz.Model (query)
import BookBrainz.Types
import BookBrainz.Types.MVC (Model)
import BookBrainz.Types.Newtypes
import Control.Applicative
import Data.Map ((!), fromList)
import Database.HDBC (toSql, fromSql)
import Data.List (intercalate)

loadForAuthorCredits :: AuthorCredit -> Model AuthorCredit
loadForAuthorCredits ac = do
  let personIds = (toSql . personId . creditedAuthor) `map` (authorCredits ac)
  people <- mapRows "id" <$> query (selectQuery personIds) personIds
  return $ ac { authorCredits = (expandCredit people)  `map` (authorCredits ac) }
  where selectQuery ids = unlines [ "SELECT *"
                                  , "FROM person"
                                  , "WHERE id IN " ++ (surround "(" ")" $ intercalate ", " $ replicate (length ids) "?")
                                  ]
        surround a b x = a ++ x ++ b
        mapRows key rows = fromList $ (\row -> ((fromSql $ row ! key)::PersonId, fromRow row)) `map` rows
        fromRow row  = Person { personName = fromSql $ row ! "name"
                              , personId = fromSql $ row ! "id"
                              , personGid = fromSql $ row ! "gid"
                              }
        expandCredit people credit = credit { creditedAuthor = people ! (personId $ creditedAuthor credit) }
