{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Control.Monad.Error
import Data.ByteString.Char8 (unpack)
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Snap.Http.Server           hiding (Config)
import Snap.Types
import Web.Routes (runSite)

import BookBrainz.Sitemap (routeSite)
import BookBrainz.Types.MVC

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
serve conn = do
    p <- getRequest >>= maybe pass return . urlDecode . rqPathInfo
    let f = runSite "/" routeSite $ unpack p
    case f of
      Right handler -> run handler
      Left e -> error e
  where run = runHandler conn
