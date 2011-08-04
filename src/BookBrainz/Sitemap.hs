{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeOperators #-}
module BookBrainz.Sitemap
       ( Sitemap(..)
       , sitemap
       , routeSite
       ) where

import Prelude hiding ((.))
import Control.Category ((.))

import Data.Maybe (fromJust)
import Data.UUID (UUID, fromString, toString)
import Text.Boomerang.TH (derivePrinterParsers)
import Web.Routes (RouteT, liftRouteT)
import Web.Routes.Boomerang

import BookBrainz.Types.MVC (Controller)
import BookBrainz.Controller.Book as Book
import BookBrainz.Controller.Person as Person

data Sitemap
     = Home
     | Book UUID
     | Person UUID
     deriving (Eq, Show)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router Sitemap
sitemap =
     rHome
  <> rBook . ("book" </> uuid)
  <> rPerson . ("person" </> uuid)

uuid = xmaph (fromJust . fromString) (Just . toString) anyString

route :: Sitemap -> RouteT Sitemap Controller ()
route url = case url of
  Home -> liftRouteT books
  Book bbid -> liftRouteT $ bookResource bbid
  Person bbid -> liftRouteT $ personResource bbid

routeSite = boomerangSiteRouteT route sitemap
