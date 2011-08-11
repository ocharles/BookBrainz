-- | Functions for working with 'BookBrainz.Types.Person.Person' entities
module BookBrainz.Model.Person
       ( insertPerson
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map               ((!))
import Data.UUID
import Database.HDBC          (fromSql, toSql)
import System.Random

import BookBrainz.Database
import BookBrainz.Model        (CoreEntity(..), coreEntityFromRow, TableName(..))
import BookBrainz.Types

instance CoreEntity Person where
  newFromRow row = Person { personName = fromSql $ row ! "name"
                          }
  tableName = TableName "person"

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
  return $ coreEntityFromRow personRow
  where insertQuery = unlines [ "INSERT INTO person (name, gid)"
                              , "VALUES (?, ?)"
                              , "RETURNING *"
                              ]
