module BookBrainz.Model.Person
       ( fromRow
       ) where

import BookBrainz.Types
import Data.Map (Map, (!))
import Database.HDBC (SqlValue, fromSql)

fromRow :: Map String SqlValue -> LoadedCoreEntity Person
fromRow row = let person = Person { personName = fromSql $ row ! "name"
                                  } in
              CoreEntity { gid            = fromSql $ row ! "gid"
                         , coreEntityInfo = person
                         , coreEntityId   = fromSql $ row ! "id"
                         }

