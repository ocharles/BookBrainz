{-# LANGUAGE OverloadedStrings #-}

import BookBrainz.Types.MVC
import BookBrainz.Model.Book
import BookBrainz.Model
import BookBrainz.Types.Newtypes
import BookBrainz.Types
import Control.Monad.Reader
import Data.Maybe (fromJust)
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Snap.Http.Server           hiding (Config)
import Snap.Types

bookResource :: Controller ()
bookResource = do
  book <- model $ getBook $ BookId 1
  writeText $ bookName $ fromJust book

runHandler :: Connection -> Controller () -> Snap ()
runHandler conn controller = runReaderT (runController controller) $ ControllerState conn

-- | Main entry point.
main :: IO ()
main = do
  connection <- connectPostgreSQL "dbname=bookbrainz user=bookbrainz"
  httpServe server (serve connection)
 where server = defaultConfig

serve :: Connection -> Snap ()
serve conn = route routes where
  routes = [("/book/:id", run bookResource)]
  run = runHandler conn
