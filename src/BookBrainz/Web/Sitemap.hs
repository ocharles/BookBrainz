{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}

{-| The URL routing table for BookBrainz, defining which handlers respond to
which URLs, and how parameters are extracted. -}
module BookBrainz.Web.Sitemap
       ( Sitemap(..)
       , sitemap
       , showURL
       ) where

import Prelude hiding                 ((.))
import Control.Category               ((.))
import Data.Maybe                     (fromJust)

import Data.UUID                      (UUID, fromString, toString)
import Text.Boomerang.TH              (derivePrinterParsers)
import Web.Routes.Base                (encodePathInfo)
import Web.Routes.Boomerang

data Sitemap
     = Home
       -- /book
     | Book UUID
     | AddBook

       -- /person
     | Person UUID

       -- /edition
     | Edition UUID
     deriving (Eq, Show)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router Sitemap
sitemap =
     rHome

  <> rAddBook . ("book" </> "add")
  <> rBook . ("book" </> uuid)

  <> rPerson . ("person" </> uuid)

  <> rEdition . ("edition" </> uuid)

uuid :: PrinterParser StringsError [String] o (UUID :- o)
uuid = xmaph (fromJust . fromString) (Just . toString) anyString

--------------------------------------------------------------------------------
-- | Turn a 'Sitemap' value into a string URL with query parameters
showURLParams :: Sitemap             -- ^ The path to convert into a URL
              -> [(String, String)]  -- ^ An association list of query
                                     --   parameters
              -> String
showURLParams url q = case unparseStrings sitemap url of
  Nothing -> error ("Could not route " ++ show url)
  Just ps -> encodePathInfo ps q

--------------------------------------------------------------------------------
-- | Turn a 'Sitemap' value into a string, using no query parameters
showURL :: Sitemap             -- ^ The path to convert into a URL
        -> String
showURL u = showURLParams u []
