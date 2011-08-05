{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module BookBrainz.Web.Snaplet.Database
    ( databaseInit
    , Database
    ) where

import Control.Monad.State      (gets)
import Control.Monad.IO.Class   (liftIO)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Snap.Snaplet

import BookBrainz.Database (Database(..), HasDatabase(..))

databaseInit :: SnapletInit b Database
databaseInit = makeSnaplet "database" "PostgreSQL database connection" Nothing $ liftIO $ do
    conn <- connectPostgreSQL "dbname=bookbrainz user=bookbrainz"
    return $ Database conn

instance HasDatabase (Handler b Database) where
  askConnection = gets connectionHandle
