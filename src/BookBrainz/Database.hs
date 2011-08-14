{-# LANGUAGE FlexibleContexts #-}

{-| Connect to a BookBrainz PostgreSQL database and interact with it.

This module is very low level, and deals with executing arbitrary SQL. You are
likely more interested in the various 'BookBrainz.Model' modules. -}
module BookBrainz.Database
       (
         Row
       , (!)
       , prefixedRow

         -- * Database Operations
       , query
       , queryOne

         -- * Connection Handling
       , HasDatabase(..)
       , Database
       , connectionHandle
       , openConnection
       ) where

import Control.Applicative      ((<$>))
import Data.List                (isPrefixOf)

import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Convertible         (Convertible)
import Data.Map                 (Map, findWithDefault, mapKeys, filterWithKey)
import Database.HDBC            (fetchAllRowsMap, prepare, execute, SqlValue
                                ,fromSql, fetchRow)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)

-- | A row is a mapping of column names to 'SqlValue's.
type Row = Map String SqlValue

--------------------------------------------------------------------------------
-- | Holds a connection to the database.
data Database = Database
    { {-| Extract the 'Connection' from a 'Database'. You will commonly not
      need to call this. Most functions in this module operate on anything that
      is an instance of the 'HasDatabase' class. -}
      connectionHandle :: Connection
    }

--------------------------------------------------------------------------------
-- | A monad that has a connection to the BookBrainz database.
class MonadIO m => HasDatabase m where
  -- | Get the @Connection@ to PostgreSQL.
  askConnection :: m Connection

--------------------------------------------------------------------------------
{-| Perform a SQL query on the database, and return a 'Map' for each row to
access the values in an order-independent manner. -}
query :: HasDatabase m
      => String                  {-^ The raw SQL to execute. Use @?@ to
                                     indicate placeholders. -}
      -> [SqlValue]              {-^ Values for each placeholder according
                                     to its position in the SQL statement. -}
      -> m [Row]                 {-^ A 'Map' of attribute name to attribute value
                                     for each row. Can be the empty list. -}
query sql bind = do
  conn <- askConnection
  liftIO $ do
    stmt <- prepare conn sql
    execute stmt bind
    fetchAllRowsMap stmt

--------------------------------------------------------------------------------
{-| Perform a SQL query on the database, and return the first column of the
first row. -}
queryOne :: (HasDatabase m, Convertible SqlValue v, Functor m)
         => String                  {-^ The raw SQL to execute. Use @?@ to
                                        indicate placeholders. -}
         -> [SqlValue]              {-^ Values for each placeholder according
                                        to its position in the SQL statement. -}
         -> m (Maybe v)             {-^ The column value. -}
queryOne sql bind = do
  conn <- askConnection
  fmap (fromSql . head) `fmap`
    liftIO (do stmt <- prepare conn sql
               execute stmt bind
               fetchRow stmt)

--------------------------------------------------------------------------------
-- | Open a connection to the BookBrainz PostgreSQL database.
openConnection :: MonadIO m => m Database  -- ^ A connected 'Database'.
openConnection =
  liftIO $ Database <$> connectPostgreSQL "dbname=bookbrainz user=bookbrainz"

--------------------------------------------------------------------------------
-- | Attempt to find the value of a column, throwing an exception if it can't
-- be found.
(!) :: (Convertible SqlValue a)
    => Row    -- ^ The row to look up a column value from.
    -> String -- ^ The name of the column to find a value for.
    -> a
row ! k = fromSql $ findWithDefault (notFound k) k row
  where notFound = error . (("IN " ++ show row ++ " could not find: ") ++)

infixl 9 !

--------------------------------------------------------------------------------
-- | Finds all columns that match a prefix, and strips the prefix.
prefixedRow :: String -- ^ The prefix to find and filter.
            -> Row    -- ^ The row to manipulate.
            -> Row    -- ^ The filtered and transformed row.
prefixedRow pre r = rewriteKey `mapKeys` (hasPrefix `filterWithKey` r)
  where hasPrefix k _ = pre `isPrefixOf` k
        rewriteKey = drop (length pre)
