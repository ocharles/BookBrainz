{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeOperators #-}

{-| The URL routing table for BookBrainz, defining which handlers respond to
which URLs, and how parameters are extracted. -}
module BookBrainz.Web.Sitemap
       ( routeSite
       ) where

import Prelude hiding                 ((.))
import Control.Category               ((.))

import Data.Maybe                     (fromJust)
import Data.UUID                      (UUID, fromString, toString)
import Text.Boomerang.TH              (derivePrinterParsers)
import Web.Routes                     (RouteT, liftRouteT)
import Web.Routes.Site                (Site)
import Web.Routes.Boomerang

import BookBrainz.Web.Handler.Book
import BookBrainz.Web.Handler.Person
import BookBrainz.Web.Snaplet         (BookBrainzHandler)

data Sitemap
     = Home
     | Book UUID
     | Person UUID
     | AddBook
     deriving (Eq, Show)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router Sitemap
sitemap =
     rHome
  <> rAddBook . ("book" </> "add")
  <> rBook . ("book" </> uuid)
  <> rPerson . ("person" </> uuid)

uuid :: PrinterParser StringsError [String] o (UUID :- o)
uuid = xmaph (fromJust . fromString) (Just . toString) anyString

route :: Sitemap -> RouteT Sitemap BookBrainzHandler ()
route url = liftRouteT $ case url of
  Home        -> listBooks
  Book bbid   -> showBook bbid
  Person bbid -> showPerson bbid
  AddBook     -> addBook

-- | A handler than routes the entire BookBrainz website.
routeSite :: Site Sitemap (BookBrainzHandler ())
routeSite = boomerangSiteRouteT route sitemap
