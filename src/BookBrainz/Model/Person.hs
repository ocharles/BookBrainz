module BookBrainz.Model.Person
       ( fromRow
       ) where

import BookBrainz.Types
import Data.Map (Map, (!))
import Database.HDBC (SqlValue, fromSql)

fromRow :: Map String SqlValue -> Person
fromRow row = Person { personName = fromSql $ row ! "name"
                     , personId = fromSql $ row ! "id"
                     , personGid = fromSql $ row ! "gid"
                     }
