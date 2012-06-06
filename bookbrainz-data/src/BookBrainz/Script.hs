{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Access the BookBrainz API from a script environment.
module BookBrainz.Script
       ( runScript
       , Script
       ) where

import Control.Applicative (Applicative)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader
import Data.Configurator        (load, require, Worth (..))
import Snap.Snaplet.PostgresqlSimple (HasPostgres(..))

import BrainzStem.Database      (Database(..), openConnection)

-- | The state accessible to the script.
data ScriptState = ScriptState {
    modelStateConn :: Database
  }

-- | The script monad.
newtype Script a = Script {
    unScript :: ReaderT ScriptState IO a
  } deriving (Monad, MonadReader ScriptState, Functor, MonadIO, MonadCatchIO, Applicative)

instance HasPostgres Script where
  getPostgresState = asks (connectionHandle . modelStateConn)

runScript :: Script a -> IO a
runScript action = do
  config <- load [Required "snaplets/database/snaplet.cfg"]
  dbName <- require config "database"
  dbUser <- require config "user"
  conn <- openConnection dbName dbUser
  runReaderT (unScript action) $ ScriptState conn
