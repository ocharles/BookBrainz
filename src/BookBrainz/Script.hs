{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Access the BookBrainz API from a script environment.
module BookBrainz.Script
       ( runScript
       , Script
       ) where

import Control.Monad.Reader
import Data.Configurator        (load, lookup, Worth (..))
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
  config <- load [Required "bookbrainz.cfg"]
  dbName <- require config "database.database"
  dbUser <- require config "database.user"
  conn <- openConnection dbName dbUser
  runReaderT (unScript action) $ ScriptState conn
