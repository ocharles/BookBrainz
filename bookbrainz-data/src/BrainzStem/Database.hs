{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Connect to a MetaBrainz PostgreSQL database and interact with it.

This module is very low level, and deals with executing arbitrary SQL. You are
likely more interested in 'BrainzStem.Model'. -}

module BrainzStem.Database
       (
         openConnection

         -- * Database Operations
       , queryOne
       , queryOne_
       , safeQueryOne
       , runDatabase

         -- * Connection Handling
       , Database(Database, connectionHandle)
       , DatabaseContext
       ) where

import Prelude hiding (catch)

import Data.Maybe               (listToMaybe)

import Control.Applicative      (Applicative, (<*))
import Control.Monad.CatchIO    (MonadCatchIO)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Control.Monad.Reader     (runReaderT, ReaderT, asks, MonadReader)
import Data.Pool (createPool)

import Database.PostgreSQL.Simple (connect, defaultConnectInfo, ConnectInfo(..), close, Query)

import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Snap.Snaplet.PostgresqlSimple (HasPostgres(..), Postgres(..), query, query_, commit)

--------------------------------------------------------------------------------
-- | Holds a connection to the database.
data Database = Database
    { {-| Extract the 'Connection' from a 'Database'. You will commonly not
      need to call this. Most functions in this module operate on anything that
      is an instance of the 'HasDatabase' class. -}
      connectionHandle :: Postgres
    }

--------------------------------------------------------------------------------
{-| Perform a SQL query on the database, and return the first column of the
first row. -}
safeQueryOne :: (Functor m, HasPostgres m, ToRow p, FromRow r)
             => Query
             -- ^ The raw SQL to execute. Use @?@ to indicate placeholders.
             -> p
             -- ^ Values for each placeholder according to its position in
             -- the SQL statement.
             -> m (Maybe r)
             -- ^ The column value.
safeQueryOne sql bind = fmap listToMaybe $ query sql bind

--------------------------------------------------------------------------------
{-| Perform a SQL query on the database, and return the first column of the
first row. -}
queryOne :: (Functor m, HasPostgres m, ToRow p, FromRow r)
         => Query
         -- ^ The raw SQL to execute. Use @?@ to indicate placeholders.
         -> p
         -- ^ Values for each placeholder according to its position in
         -- the SQL statement.
         -> m r
         -- ^ The column value.
queryOne sql bind = fmap head $ query sql bind

{-| Perform a SQL query on the database, and return the first column of the
first row. -}
queryOne_ :: (Functor m, HasPostgres m, FromRow r)
          => Query
          -- ^ The raw SQL to execute. Use @?@ to indicate placeholders.
          -> m r
          -- ^ The column value.
queryOne_ sql = fmap head $ query_ sql

--------------------------------------------------------------------------------
-- | Open a connection to the BookBrainz PostgreSQL database.
openConnection :: MonadIO m
               => String      -- ^ The database name.
               -> String      -- ^ The user to connect as.
               -> m Database  -- ^ A connected 'Database'.
openConnection dbName dbUser = liftIO $ do
  createPool connector disconnector 1 5 20 >>= return . Database . Postgres
  where connector = connect defaultConnectInfo { connectDatabase = dbName
                                               , connectUser = dbUser
                                               }
        disconnector = close

--------------------------------------------------------------------------------
-- | Run some IO code with a context that has access to a database.
newtype DatabaseContext a = DatabaseContext {
      runDbAction :: ReaderT Database IO a
  } deriving (Monad, MonadIO, MonadCatchIO, MonadReader Database, Functor, Applicative)

instance HasPostgres DatabaseContext where
  getPostgresState = asks connectionHandle

runDatabase :: Database -> DatabaseContext a -> IO a
runDatabase db action = do
  ret <- runReaderT (runDbAction (action <* commit)) db
  return ret
