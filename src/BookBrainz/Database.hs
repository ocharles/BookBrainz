{-| Connect to a BookBrainz PostgreSQL and interact with it.

This module is very low level, and deals with executing arbitrary SQL. You are
likely more interested in the various 'BookBrainz.Model' modules. -}
module BookBrainz.Database
       (
         Row

         -- * Database Operations
       , query

         -- * Connection Handling
       , HasDatabase(..)
       , Database
       , connectionHandle
       , openConnection
       ) where

import Control.Applicative      ((<$>))

import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Map                 (Map)
import Database.HDBC            (fetchAllRowsMap, prepare, execute, SqlValue)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)

-- | A row is a mapping of column names to 'SqlValue's.
type Row = Map String SqlValue

--------------------------------------------------------------------------------
-- | Holds a connection to the database.
data Database = Database
    { {-| Extract the 'Connection' from a 'Database'. You will commonly not
      need to call this, most functions in this module operate on anything that
      is an instance of the 'HasDatabase' class. -}
      connectionHandle :: Connection
    }

--------------------------------------------------------------------------------
-- | A monad that has a connection to the BookBrainz database.
class MonadIO m => HasDatabase m where
  -- | Get the @Connection@ to PostgreSQL
  askConnection :: m Connection

--------------------------------------------------------------------------------
{-| Perform a SQL query on the database, and return a 'Map' for each row to
access the values in an order independant manner. -}
query :: HasDatabase m
      => String                  {-^ The raw SQL to execute. Use @?@ to
                                     indicate placeholders. -}
      -> [SqlValue]              {-^ Values for each placeholder, according
                                     to its position in the SQL statement. -}
      -> m [Row]                 {-^ A Map of attribute name to attribute value
                                     for each row. May be the empty list. -}
query sql bind = do
  conn <- askConnection
  liftIO $ do
    stmt <- prepare conn sql
    execute stmt bind
    fetchAllRowsMap stmt

--------------------------------------------------------------------------------
-- | Open a connection to the BookBrainz PostgreSQL database.
openConnection :: MonadIO m => m Database  -- ^ A connected 'Database'.
openConnection =
  liftIO $ Database <$> connectPostgreSQL "dbname=bookbrainz user=bookbrainz"
