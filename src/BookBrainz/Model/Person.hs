module BookBrainz.Model.Person
       ( personFromRow
       , insertPerson
       , getPerson
       ) where

import BookBrainz.Model
import BookBrainz.Types
import Data.Map (Map, (!))
import Data.Maybe
import Data.UUID
import Database.HDBC (SqlValue, fromSql, toSql)
import System.Random

personFromRow :: Map String SqlValue -> LoadedCoreEntity Person
personFromRow row = let person = Person { personName = fromSql $ row ! "name"
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
  return $ personFromRow personRow
  where insertQuery = unlines [ "INSERT INTO person (name, gid)"
                              , "VALUES (?, ?)"
                              , "RETURNING *"
                              ]

getPerson :: UUID -> Model (Maybe (LoadedCoreEntity Person))
getPerson bbid = do
  results <- query selectQuery [ toSql bbid ]
  return $ personFromRow `fmap` listToMaybe results
  where selectQuery = unlines  [ "SELECT *"
                               , "FROM person"
                               , "WHERE gid = ?" ]
