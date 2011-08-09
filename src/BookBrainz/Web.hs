{-# LANGUAGE OverloadedStrings #-}

-- | The BookBrainz web frontend
module BookBrainz.Web
       ( bookbrainz
       ) where

import Control.Monad.CatchIO           (tryJust)
import Data.ByteString.Char8           (unpack)
import Snap.Snaplet
import Snap.Types
import Web.Routes                      (runSite)

import BookBrainz.Web.Handler          (HttpError(..), output)
import BookBrainz.Web.Sitemap          (routeSite)
import BookBrainz.Web.Snaplet
import BookBrainz.Web.Snaplet.Database
import qualified BookBrainz.Web.View as V

--------------------------------------------------------------------------------
-- | Initialize the 'BookBrainz' 'Snap.Snaplet'
bookbrainz :: SnapletInit BookBrainz BookBrainz
bookbrainz = makeSnaplet "bookbrainz" "BookBrainz" Nothing $ do
    db <- nestSnaplet "database" database databaseInit
    addRoutes [ ("", site) ]
    return $ makeBbSnaplet db
  where site = do
          p <- getRequest >>= maybe pass return . urlDecode . rqPathInfo
          case runSite "/" routeSite $ unpack p of
            Right handler -> runHandler handler
            Left e -> error e

runHandler :: BookBrainzHandler () -> BookBrainzHandler ()
runHandler a = do
  outcome <- tryJust errorH a
  case outcome of
    Right r -> return r
    Left h' -> h'
  where errorH (Http404 message) =
          (Just . output . V.genericError) message
