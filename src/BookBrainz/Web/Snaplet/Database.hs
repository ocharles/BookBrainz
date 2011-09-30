{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

-- | A snaplet giving access to the BookBrainz 'Database'.
module BookBrainz.Web.Snaplet.Database
    ( databaseInit
    , Database
    , withTransaction
    ) where

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.State.Class (gets)
import           Data.Configurator         (require)
import           Data.Lens.Common          (Lens)
import qualified Database.HDBC             as HDBC
import           Snap.Snaplet

import BrainzStem.Database (Database, HasDatabase(..), connectionHandle
                           ,openConnection)

--------------------------------------------------------------------------------
-- | Initialize the database snaplet with a new connection.
databaseInit :: SnapletInit b Database
databaseInit = makeSnaplet "database" "PostgreSQL database connection"
                           Nothing $ do
    config <- getSnapletUserConfig
    liftIO $ conn config
  where conn config = do db <- require config "database"
                         user <- require config "user"
                         openConnection db user


instance HasDatabase (Handler b Database) where
  askConnection = gets connectionHandle

--------------------------------------------------------------------------------
-- | Run a handler action within the scope of a transaction.
withTransaction :: Lens b (Snaplet Database)    -- ^ A lens to the database snaplet.
                -> Handler b v a                -- ^ The handler to execute.
                -> Handler b v a
withTransaction l h = do
  a <- h
  withTop l commit'
  return a
  where commit' = do
          c <- askConnection
          liftIO $ HDBC.commit c
