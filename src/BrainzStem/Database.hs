{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| Connect to a MetaBrainz PostgreSQL database and interact with it.

This module is very low level, and deals with executing arbitrary SQL. You are
likely more interested in 'BrainzStem.Model'. -}

module BrainzStem.Database
       (
         prefixedRow
       , (!)
       , openConnection

         -- * Database Operations
       , queryOne
       , safeQueryOne
       , runDatabase

         -- * Connection Handling
       , Database(Database)
       ) where

import Prelude hiding (catch)

import Data.List                (isPrefixOf)
import Data.Maybe               (fromJust)

import Control.Applicative      (Applicative)
import Control.Monad.IO.Control (MonadControlIO)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Control.Monad.Reader     (runReaderT, ReaderT, asks, MonadReader)
import Data.Convertible         (Convertible, safeConvert, ConvertError(..))
import Data.Map                 (findWithDefault, mapKeys, filterWithKey)
import Database.HDBC            (prepare, execute, SqlValue
                                ,fromSql, fetchRow, toSql, commit)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Snap.Snaplet.Hdbc        (HasHdbc(..), withHdbc, Row)

import BrainzStem.Types

--------------------------------------------------------------------------------
-- | Holds a connection to the database.
data Database = Database
    { {-| Extract the 'Connection' from a 'Database'. You will commonly not
      need to call this. Most functions in this module operate on anything that
      is an instance of the 'HasDatabase' class. -}
      connectionHandle :: Connection
    }

--------------------------------------------------------------------------------
{-| Perform a SQL query on the database, and return the first column of the
first row. -}
safeQueryOne :: HasHdbc m c s
             => String
             -- ^ The raw SQL to execute. Use @?@ to indicate placeholders.
             -> [SqlValue]
             -- ^ Values for each placeholder according to its position in
             -- the SQL statement.
             -> m (Maybe SqlValue)
             -- ^ The column value.
safeQueryOne sql bind = withHdbc $ \conn ->
  fmap head `fmap` (do stmt <- prepare conn sql
                       execute stmt bind
                       fetchRow stmt)

--------------------------------------------------------------------------------
{-| Perform a SQL query on the database, and return the first column of the
first row. -}
queryOne :: HasHdbc m c s
         => String
         -- ^ The raw SQL to execute. Use @?@ to indicate placeholders.
         -> [SqlValue]
         -- ^ Values for each placeholder according to its position in
         -- the SQL statement.
         -> m SqlValue
         -- ^ The column value.
queryOne sql bind = withHdbc $ \conn -> do
  head `fmap` (do stmt <- prepare conn sql
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

instance Convertible SqlValue (BBID a) where
  safeConvert bbid' = maybe (convError' bbid') return (parseBbid $ fromSql bbid')
    where convError' inpval =
            Left $ ConvertError { convSourceValue = show inpval
                                , convSourceType = "SqlValue"
                                , convDestType = "BBID"
                                , convErrorMessage = "Not a valid BBID"
                                }

instance Convertible (BBID a) SqlValue where
  safeConvert = Right . toSql . show

instance Convertible (Ref a) SqlValue where
  safeConvert = Right . rowKey

instance Convertible SqlValue (Ref a) where
  safeConvert id' = Right $ Ref id'

--------------------------------------------------------------------------------
-- | Run some IO code with a context that has access to a database.
newtype DatabaseContext a = DatabaseContext {
      runDbAction :: ReaderT Database IO a
  } deriving (Monad, MonadIO, MonadReader Database, Functor, Applicative,
              MonadControlIO)

instance HasHdbc DatabaseContext Connection IO where
  getConnSrc = asks (return . connectionHandle)

runDatabase :: Database -> DatabaseContext a -> IO a
runDatabase db action = do
  ret <- runReaderT (runDbAction action) db
  commit $ connectionHandle db
  return ret
