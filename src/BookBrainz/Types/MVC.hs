{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BookBrainz.Types.MVC
       ( Controller(..)
       , ControllerState(..)
       , ModelState(..)
       , Model(..)
       ) where

import Control.Applicative        (Applicative,Alternative)
import Control.Monad              (MonadPlus)
import Control.Monad.CatchIO      (MonadCatchIO)
import Control.Monad.Reader       (ReaderT,MonadReader)
import Control.Monad.Error        (ErrorT,MonadError)
import Control.Monad.Trans        (MonadIO)
import Database.HDBC.PostgreSQL   (Connection)
import Snap.Types                 (Snap,MonadSnap)

-- | The state accessible to the controller (DB/session stuff).
data ControllerState = ControllerState {
    controllerStateConn :: Connection
  }

-- | The controller monad.
newtype Controller a = Controller {
    runController :: ReaderT ControllerState (ErrorT String Snap) a
  } deriving (Monad
             ,Functor
             ,Applicative
             ,Alternative
             ,MonadReader ControllerState
             ,MonadError String
             ,MonadSnap
             ,MonadIO
             ,MonadCatchIO
             ,MonadPlus)

-- | The state accessible to the model (just DB connection).
data ModelState = ModelState {
    modelStateConn :: Connection
  }

-- | The model monad (limited access to IO, only DB access).
newtype Model a = Model {
    runModel :: ReaderT ModelState IO a
  } deriving (Monad, MonadReader ModelState, Functor, Applicative)
