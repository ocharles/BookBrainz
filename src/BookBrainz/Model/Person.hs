-- | Functions for working with 'BookBrainz.Types.Person.Person' entities.
module BookBrainz.Model.Person
       ( insertPerson
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID
import Database.HDBC          (toSql)
import System.Random

import BrainzStem.Database     (HasDatabase, query, (!))
import BrainzStem.Model        (CoreEntity(..), HasTable(..), coreEntityFromRow
                               ,TableName(..))
import BookBrainz.Types

instance HasTable Person where
  newFromRow row = Person { personName = row ! "name"
                          }
  tableName = TableName "person"

instance CoreEntity Person

--------------------------------------------------------------------------------
-- | Insert and version a new 'Person'.
insertPerson :: (Functor m, HasDatabase m)
             => Person                      {-^ The information about the person to
                                            insert. -}
             -> m (LoadedCoreEntity Person) {-^ The person, loaded from the database
                                            (complete with GID). -}
insertPerson personSpec = do
  personGid <- liftIO randomIO :: MonadIO m => m UUID
  personRow <- head `fmap` query insertQuery [ toSql $ personName personSpec
                                             , toSql   personGid
                                             ]
  return $ coreEntityFromRow personRow
  where insertQuery = unlines [ "INSERT INTO person (name, gid)"
                              , "VALUES (?, ?)"
                              , "RETURNING *"
                              ]
