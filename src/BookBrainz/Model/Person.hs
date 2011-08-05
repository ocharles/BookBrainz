module BookBrainz.Model.Person
       ( personFromRow
       , insertPerson
       , getPerson
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map, (!))
import Data.Maybe
import Data.UUID
import Database.HDBC (SqlValue, fromSql, toSql)
import System.Random

import BookBrainz.Database
import BookBrainz.Types

personFromRow :: Map String SqlValue -> LoadedCoreEntity Person
personFromRow row = let person = Person { personName = fromSql $ row ! "name"
                                  } in
              CoreEntity { gid               = fromSql $ row ! "gid"
                         , coreEntityInfo    = person
                         , coreEntityVersion = fromSql $ row ! "version"
                         }

insertPerson :: (Functor m, HasDatabase m) => Person -> m (LoadedCoreEntity Person)
insertPerson personSpec = do
  personGid <- liftIO randomIO :: MonadIO m => m UUID
  personRow <- head `fmap` query insertQuery [ toSql $ personName personSpec
                                             , toSql   personGid
                                             ]
  return $ personFromRow personRow
  where insertQuery = unlines [ "INSERT INTO person (name, gid)"
                              , "VALUES (?, ?)"
                              , "RETURNING *"
                              ]

getPerson :: (Functor m, HasDatabase m) => UUID -> m (Maybe (LoadedCoreEntity Person))
getPerson bbid = do
  results <- query selectQuery [ toSql bbid ]
  return $ personFromRow `fmap` listToMaybe results
  where selectQuery = unlines  [ "SELECT *"
                               , "FROM person"
                               , "WHERE gid = ?" ]
