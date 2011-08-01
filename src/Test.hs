{-# LANGUAGE OverloadedStrings #-}

import BookBrainz.Controller.Book
import BookBrainz.Controller.Person
import BookBrainz.Types.MVC
import Control.Monad.Reader
import Control.Monad.Error

import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Snap.Http.Server           hiding (Config)
import Snap.Types

runHandler :: Connection -> Controller () -> Snap ()
runHandler conn controller = do
  modifyResponse $ setContentType "text/html; charset=utf8"
  outcome <- runErrorT $ runReaderT (runController controller) $ ControllerState conn
  handleOutcome outcome
  where handleOutcome (Left msg) = error msg
        handleOutcome (Right outcome) = return outcome

-- | Main entry point.
main :: IO ()
main = do
  connection <- connectPostgreSQL "dbname=bookbrainz user=bookbrainz"
  httpServe server (serve connection)
 where server = defaultConfig

serve :: Connection -> Snap ()
serve conn = route routes where
  routes = [ ("/books/add", run $ methods [POST, GET] addBook)
           , ("/books/", run books)
           , ("/book/:gid", run bookResource)
           , ("/person/:gid", run personResource)
           ]
  run = runHandler conn
