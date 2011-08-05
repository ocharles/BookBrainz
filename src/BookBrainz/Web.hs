{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web where

import Data.ByteString.Char8 (unpack)
import Snap.Snaplet
import Snap.Types
import Web.Routes (runSite)

import BookBrainz.Web.Handler (runHandler)
import BookBrainz.Web.Sitemap (routeSite)
import BookBrainz.Web.Snaplet
import BookBrainz.Web.Snaplet.Database

bookbrainz :: SnapletInit BookBrainz BookBrainz
bookbrainz = makeSnaplet "bookbrainz" "BookBrainz" Nothing $ do
    db <- nestSnaplet "database" database databaseInit
    addRoutes [ ("", site) ]
    return $ BookBrainz db
  where site = do
          p <- getRequest >>= maybe pass return . urlDecode . rqPathInfo
          case runSite "/" routeSite $ unpack p of
            Right handler -> runHandler handler
            Left e -> error e
