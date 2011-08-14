{-# LANGUAGE OverloadedStrings #-}

-- | The BookBrainz web frontend.
module BookBrainz.Web
       ( bookbrainz
       ) where

import Control.Monad.CatchIO           (tryJust)
import Data.ByteString.Char8           (unpack)
import Snap.Snaplet
import Snap.Types
import Snap.Util.FileServe
import Web.Routes                      (runSite, RouteT, liftRouteT)
import Web.Routes.Site                 (Site)
import Web.Routes.Boomerang

import BookBrainz.Web.Handler          (HttpError(..), output)
import BookBrainz.Web.Handler.Book
import BookBrainz.Web.Handler.Edition
import BookBrainz.Web.Handler.Person
import BookBrainz.Web.Handler.Publisher
import BookBrainz.Web.Sitemap          (Sitemap(..), sitemap)
import BookBrainz.Web.Snaplet
import BookBrainz.Web.Snaplet.Database
import qualified BookBrainz.Web.View as V

routeUrl :: Sitemap -> RouteT Sitemap BookBrainzHandler ()
routeUrl url = liftRouteT $ case url of
  Home           -> listBooks
  Resource _     -> error "Resource should have been served by Snap"
  Book bbid      -> showBook bbid
  AddBook        -> addBook
  Person bbid    -> showPerson bbid
  Edition bbid   -> showEdition bbid
  Publisher bbid -> showPublisher bbid

-- | A handler that routes the entire BookBrainz website.
routeSite :: Site Sitemap (BookBrainzHandler ())
routeSite = boomerangSiteRouteT routeUrl sitemap

--------------------------------------------------------------------------------
-- | Initialize the 'BookBrainz' 'Snap.Snaplet'.
bookbrainz :: SnapletInit BookBrainz BookBrainz
bookbrainz = makeSnaplet "bookbrainz" "BookBrainz" Nothing $ do
    db <- nestSnaplet "database" database databaseInit
    addRoutes [ ("/static", serveDirectory "resources")
              , ("", site) ]
    return $ makeBbSnaplet db
  where site = do
          p <- getRequest >>= maybe pass return . urlDecode . rqPathInfo
          case runSite "/" routeSite $ unpack p of
            Right handler -> runHandler handler
            Left e -> error e

runHandler :: BookBrainzHandler () -> BookBrainzHandler ()
runHandler a = do
  modifyResponse $ setContentType "text/html; charset=utf8"
  outcome <- tryJust errorH a
  case outcome of
    Right r -> return r
    Left h' -> h'
  where errorH (Http404 message) =
          (Just . output . V.genericError) message
