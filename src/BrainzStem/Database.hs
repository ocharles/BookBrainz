{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| Connect to a MetaBrainz PostgreSQL database and interact with it.

This module is very low level, and deals with executing arbitrary SQL. You are
likely more interested in 'BrainzStem.Model'. -}

module BrainzStem.Database
       (
         Row
       , (!)
       , prefixedRow

         -- * Database Operations
       , query
       , queryOne
       , safeQueryOne
       , runDatabase
       , withTransaction

         -- * Connection Handling
       , openConnection
       , HasDatabase(..)
       , Database(Database)
       , connectionHandle
       ) where

import Prelude hiding (catch)

import Control.Exception        (SomeException)
import Data.List                (isPrefixOf)
import Data.Maybe               (fromJust)

import Control.Applicative      (Applicative)
import Control.Monad.CatchIO    (onException, MonadCatchIO, catch)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Control.Monad.Reader     (runReaderT, ReaderT, asks, MonadReader)
import Data.Convertible         (Convertible, safeConvert, convError)
import Data.Map                 (Map, findWithDefault, mapKeys, filterWithKey)
import Database.HDBC            (fetchAllRowsMap, prepare, execute, SqlValue
                                ,fromSql, fetchRow, toSql, commit, rollback)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)

import BrainzStem.Types

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
-- | A monad that has a connection to a MetaBrainz database.
class (Functor m, Monad m, Applicative m, MonadIO m, MonadCatchIO m) => HasDatabase m where
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
safeQueryOne :: HasDatabase m
             => String
             -- ^ The raw SQL to execute. Use @?@ to indicate placeholders.
             -> [SqlValue]
             -- ^ Values for each placeholder according to its position in
             -- the SQL statement.
             -> m (Maybe SqlValue)
             -- ^ The column value.
safeQueryOne sql bind = do
  conn <- askConnection
  fmap head `fmap`
    liftIO (do stmt <- prepare conn sql
               execute stmt bind
               fetchRow stmt)

--------------------------------------------------------------------------------
{-| Perform a SQL query on the database, and return the first column of the
first row. -}
queryOne :: HasDatabase m
         => String
         -- ^ The raw SQL to execute. Use @?@ to indicate placeholders.
         -> [SqlValue]
         -- ^ Values for each placeholder according to its position in
         -- the SQL statement.
         -> m SqlValue
         -- ^ The column value.
queryOne sql bind = do
  conn <- askConnection
  head `fmap`
    liftIO (do stmt <- prepare conn sql
               execute stmt bind
               fromJust `fmap` fetchRow stmt)

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

--------------------------------------------------------------------------------
-- | Open a connection to the BookBrainz PostgreSQL database.
openConnection :: MonadIO m
               => String      -- ^ The database name.
               -> String      -- ^ The user to connect as.
               -> m Database  -- ^ A connected 'Database'.
openConnection dbName dbUser = liftIO $ do
  conn <- connectPostgreSQL $ connStr [ ("dbname", dbName), ("user", dbUser) ]
  return $ Database conn
  where connStr = unwords . map stringPair
        stringPair (k, v) = k ++ "=" ++ v

instance Convertible SqlValue BBID where
  safeConvert bbid' = maybe (convError "Not a valid BBID" bbid') return (parseBbid $ fromSql bbid')

instance Convertible BBID SqlValue where
  safeConvert = Right . toSql . show

instance Convertible (Ref a) SqlValue where
  safeConvert = Right . rowKey

instance Convertible SqlValue (Ref a) where
  safeConvert id' = Right $ Ref id'

--------------------------------------------------------------------------------
-- | Run an action within a transaction. If it does not cleanly execute (throws
-- exceptions) then the transaction is rolled back. Otherwise, it is commited
withTransaction :: HasDatabase m => m a -> m a
withTransaction action = do
  conn <- askConnection
  r <- onException action (liftIO $ doRollback conn)
  liftIO $ commit conn
  return r
  where doRollback conn = catch (rollback conn) doRollbackHandler
        doRollbackHandler :: SomeException -> IO ()
        doRollbackHandler _ = return ()

--------------------------------------------------------------------------------
-- | Run some IO code with a context that has access to a database.
newtype DatabaseContext a = DatabaseContext {
      runDbAction :: ReaderT Database IO a
  } deriving (Monad, MonadIO, MonadReader Database, Functor, Applicative,
              MonadCatchIO)

instance HasDatabase DatabaseContext where
  askConnection = asks connectionHandle

runDatabase :: Database -> DatabaseContext a -> IO a
runDatabase db action = do
  ret <- runReaderT (runDbAction action) db
  commit $ connectionHandle db
  return ret
