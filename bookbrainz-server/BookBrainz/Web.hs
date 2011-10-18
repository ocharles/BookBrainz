{-# LANGUAGE OverloadedStrings #-}

-- | The BookBrainz web frontend.
module BookBrainz.Web
       ( bookbrainz
       ) where

import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.CatchIO                       (tryJust)
import           Data.ByteString.Char8                       (unpack)
import           Data.Configurator                           (require)
import           Database.HDBC.PostgreSQL                    (connectPostgreSQL)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth                           (defAuthSettings
                                                             ,loginByRememberToken)
import           Snap.Snaplet.Auth.Backend.BookBrainz
import           Snap.Snaplet.Hdbc                           (hdbcInit)
import           Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import           Snap.Util.FileServe
import           Web.Routes                                  (runSite, RouteT, liftRouteT)
import           Web.Routes.Boomerang
import           Web.Routes.Site                             (Site)

import           BookBrainz.Web.Handler                      (HttpError(..), output)
import           BookBrainz.Web.Handler.Book
import           BookBrainz.Web.Handler.Edition
import           BookBrainz.Web.Handler.Person
import           BookBrainz.Web.Handler.Publisher
import           BookBrainz.Web.Handler.Search
import           BookBrainz.Web.Handler.User
import           BookBrainz.Web.Sitemap                      (Sitemap(..), sitemap)
import           BookBrainz.Web.Snaplet
import qualified BookBrainz.Web.View                         as V

routeUrl :: Sitemap -> RouteT Sitemap BookBrainzHandler ()
routeUrl url = liftRouteT $ case url of
  Home             -> listBooks
  Resource _       -> error "Resource should have been served by Snap"
  Book bbid        -> showBook bbid
  AddBook          -> addBook
  EditBook bbid    -> editBook bbid
  AddBookRole bbid -> addBookRole bbid
  Person bbid      -> showPerson bbid
  AddPerson        -> addPerson
  AddEdition bbid  -> addEdition bbid
  Edition bbid     -> showEdition bbid
  EditEdition bbid -> editEdition bbid
  AddEditionRole bbid -> addEditionRole bbid
  Publisher bbid   -> showPublisher bbid
  AddPublisher     -> addPublisher
  Search           -> search
  Login            -> login
  Register         -> register
  Logout           -> logout

-- | A handler that routes the entire BookBrainz website.
routeSite :: Site Sitemap (BookBrainzHandler ())
routeSite = boomerangSiteRouteT routeUrl sitemap

--------------------------------------------------------------------------------
-- | Initialize the 'BookBrainz' 'Snap.Snaplet'.
bookbrainz :: SnapletInit BookBrainz BookBrainz
bookbrainz = makeSnaplet "bookbrainz" "BookBrainz" Nothing $ do
    config <- getSnapletUserConfig
    conn <- liftIO $ connectToDatabase config
    dbSnaplet <- nestSnaplet "hdbc" database $ hdbcInit conn
    sessionSnaplet <- nestSnaplet "session" session cookieSessionManager
    authSnaplet <- nestSnaplet "auth" auth $
      initBookBrainzAuthManager defAuthSettings session conn
    addRoutes [ ("/static", serveDirectory "resources")
              , ("", site) ]
    wrapHandlers tryLogin
    return $ makeBbSnaplet dbSnaplet sessionSnaplet authSnaplet
  where site = do
          p <- getRequest >>= maybe pass return . urlDecode . rqPathInfo
          case runSite "/" routeSite $ unpack p of
            Right handler -> runHandler handler
            Left e -> error e
        cookieSessionManager =
          initCookieSessionManager "site_key.txt" "_bbsession" Nothing
        tryLogin h = (with auth $ loginByRememberToken) >> h
        connectToDatabase config = do
          dbName <- require config "database"
          dbUser <- require config "user"
          connectPostgreSQL $ connStr [ ("dbname", dbName), ("user", dbUser) ]
        connStr = unwords . map stringPair
        stringPair (k, v) = k ++ "=" ++ v

runHandler :: BookBrainzHandler () -> BookBrainzHandler ()
runHandler a = do
  outcome <- tryJust errorH a
  case outcome of
    Right r -> return r
    Left h' -> h'
  where errorH (Http404 message) =
          (Just . output . V.genericError) message
