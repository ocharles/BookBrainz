{-# LANGUAGE OverloadedStrings #-}

import BookBrainz.Controller.Book
import BookBrainz.Types.MVC
import Control.Monad.Reader

import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Snap.Http.Server           hiding (Config)
import Snap.Types

runHandler :: Connection -> Controller () -> Snap ()
runHandler conn controller = do
  modifyResponse $ setContentType "text/html; charset=utf8"
  runReaderT (runController controller) $ ControllerState conn

-- | Main entry point.
main :: IO ()
main = do
  connection <- connectPostgreSQL "dbname=bookbrainz user=bookbrainz"
  httpServe server (serve connection)
 where server = defaultConfig

serve :: Connection -> Snap ()
serve conn = route routes where
  routes = [("/book/:gid", run bookResource)]
  run = runHandler conn
