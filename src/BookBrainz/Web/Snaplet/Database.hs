{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeOperators #-}
module BookBrainz.Web.Snaplet.Database
    ( databaseInit
    , Database
    , withTransaction
    ) where

import Control.Monad.State      (gets)
import Control.Monad.IO.Class   (liftIO)
import Data.Record.Label        ((:->))
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import qualified Database.HDBC as HDBC
import Snap.Snaplet

import BookBrainz.Database (Database(..), HasDatabase(..))

databaseInit :: SnapletInit b Database
databaseInit = makeSnaplet "database" "PostgreSQL database connection" Nothing $ liftIO $ do
    conn <- connectPostgreSQL "dbname=bookbrainz user=bookbrainz"
    return $ Database conn

instance HasDatabase (Handler b Database) where
  askConnection = gets connectionHandle

withTransaction :: (b :-> Snaplet Database) -> Handler b v a -> Handler b v a
withTransaction l h = do
  a <- h
  withTop l commit'
  return a
  where commit' = do
          c <- askConnection
          liftIO $ HDBC.commit c
