{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Access the BookBrainz API from a script environment.
module BookBrainz.Script
       ( runScript
       , Script
       ) where

import Control.Monad.Reader
import Data.Configurator        (load, require, Worth (..))
import Database.HDBC.PostgreSQL (Connection)

import BrainzStem.Database      (HasDatabase(..), Database(..), openConnection)

-- | The state accessible to the script.
data ScriptState = ScriptState {
    modelStateConn :: Database
  }

-- | The script monad.
newtype Script a = Script {
    unScript :: ReaderT ScriptState IO a
  } deriving (Monad, MonadReader ScriptState, Functor, MonadIO)

instance HasDatabase Script where
  askConnection = asks (connectionHandle . modelStateConn)

runScript action = do
  config <- load [Required "snaplets/database/snaplet.cfg"]
  dbName <- require config "database"
  dbUser <- require config "user"
  conn <- openConnection dbName dbUser
  runReaderT (unScript action) $ ScriptState conn
