module BookBrainz.Database
       ( HasDatabase(..)
       , Database(..)
       , query
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import Database.HDBC (fetchAllRowsMap, prepare, execute, SqlValue)
import Database.HDBC.PostgreSQL (Connection)

data Database = Database { connectionHandle :: Connection }

class (Monad m, MonadIO m) => HasDatabase m where
  askConnection :: m Connection

query :: HasDatabase m => String -> [SqlValue] -> m [Map String SqlValue]
query sql bind = do
  conn <- askConnection
  liftIO $ do
    stmt <- prepare conn sql
    execute stmt bind
    fetchAllRowsMap stmt
