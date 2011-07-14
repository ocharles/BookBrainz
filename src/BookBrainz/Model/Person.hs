module BookBrainz.Model.Person
       ( fromRow
       , insertPerson
       ) where

import BookBrainz.Model
import BookBrainz.Types
import BrainzStem.Types
import Data.Map (Map, (!))
import Data.UUID
import Database.HDBC (SqlValue, fromSql, toSql)
import System.Random

fromRow :: Map String SqlValue -> LoadedCoreEntity Person
fromRow row = let person = Person { personName = fromSql $ row ! "name"
                                  } in
              CoreEntity { gid            = fromSql $ row ! "gid"
                         , coreEntityInfo = person
                         , coreEntityId   = fromSql $ row ! "id"
                         }

insertPerson :: Person -> Model (LoadedCoreEntity Person)
insertPerson personSpec = do
  personGid <- modelIO randomIO :: Model UUID
  personRow <- head `fmap` query insertQuery [ toSql $ personName personSpec
                                             , toSql   personGid
                                             ]
  return $ fromRow personRow
  where insertQuery = unlines [ "INSERT INTO person (name, gid)"
                              , "VALUES (?, ?)"
                              , "RETURNING *"
                              ]
