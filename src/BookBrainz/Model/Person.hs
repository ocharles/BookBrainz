-- | Functions for working with 'BookBrainz.Types.Person.Person' entities
module BookBrainz.Model.Person
       ( personFromRow
       , insertPerson
       , getPerson
       ) where

import Data.Maybe

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map               (Map, (!))
import Data.UUID
import Database.HDBC          (SqlValue, fromSql, toSql)
import System.Random

import BookBrainz.Database
import BookBrainz.Types

--------------------------------------------------------------------------------
{-| Create a 'Person' value from a row in the database. The Person is wrapped in
'LoadedCoreEntity' context, and will be complete with GID. -}
personFromRow :: Map String SqlValue  -- ^ A 'Map' of attribute names to values.
              -> LoadedCoreEntity Person
personFromRow row = let person = Person { personName = fromSql $ row ! "name"
                                  } in
              CoreEntity { gid               = fromSql $ row ! "gid"
                         , coreEntityInfo    = person
                         , coreEntityVersion = fromSql $ row ! "version"
                         }

--------------------------------------------------------------------------------
-- | Insert and version a new 'Person'.
insertPerson :: (Functor m, HasDatabase m)
             => Person                      {-^ The person to store and
                                            version -}
             -> m (LoadedCoreEntity Person) {-^ The now versioned Person,
                                            complete with GID -}
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

--------------------------------------------------------------------------------
-- | Get a single 'Person' by GID.
getPerson :: (Functor m, HasDatabase m)
          => UUID                                {-^ The GID of the person to
                                                 load -}
          -> m (Maybe (LoadedCoreEntity Person)) {-^ The loaded person, or
                                                 'Nothing' if the person could
                                                 not be found. -}
getPerson bbid = do
  results <- query selectQuery [ toSql bbid ]
  return $ personFromRow `fmap` listToMaybe results
  where selectQuery = unlines  [ "SELECT *"
                               , "FROM person"
                               , "WHERE gid = ?" ]
