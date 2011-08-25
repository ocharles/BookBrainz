{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Access the BookBrainz API from a script environment.
module BookBrainz.Script
       ( runScript
       , Script
       ) where

import Control.Monad.Reader
import Database.HDBC.PostgreSQL (Connection)

import BookBrainz.Database      (openConnection, HasDatabase (..), Database (..))

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
  conn <- openConnection
  runReaderT (unScript action) $ ScriptState conn
