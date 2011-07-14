module BookBrainz.Model.Person
       ( fromRow
       ) where

import BookBrainz.Types
import Data.Map (Map, (!))
import Database.HDBC (SqlValue, fromSql)

fromRow :: Map String SqlValue -> WithGid Person
fromRow row = let person = Person { personName = fromSql $ row ! "name"
                                  , personId = fromSql $ row ! "id"
                                  } in
              WithGid { gid = fromSql $ row ! "gid"
                      , info = person
                      }

