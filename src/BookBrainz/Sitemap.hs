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

data Sitemap
     = Home
     | Book UUID
     deriving (Eq, Show)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router Sitemap
sitemap =
     rHome
  <> rBook . ("book" </> uuid)

uuid = xmaph (fromJust . fromString) (Just . toString) anyString

route :: Sitemap -> RouteT Sitemap Controller ()
route url = case url of
  Home -> liftRouteT books
  Book bbid -> liftRouteT $ bookResource bbid

routeSite = boomerangSiteRouteT route sitemap
