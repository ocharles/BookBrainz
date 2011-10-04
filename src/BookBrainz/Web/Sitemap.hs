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
       , showURLParams
       ) where

import Prelude hiding       ((.))
import Control.Category     ((.))
import Data.Maybe           (fromJust)

import Text.Boomerang.TH    (derivePrinterParsers)
import Web.Routes.Base      (encodePathInfo)
import Web.Routes.Boomerang

import BookBrainz.Types

data Sitemap
     = Home

       -- /static
     | Resource String

       -- /book
     | Book (BBID Book)
     | AddBook
     | EditBook (BBID Book)
     | AddEdition (BBID Book)

       -- /person
     | Person (BBID Person)

       -- /edition
     | Edition (BBID Edition)

       -- /publisher
     | Publisher (BBID Publisher)

       -- /search
     | Search

       -- User stuff
     | Login
     | Register
     | Logout
     deriving (Eq, Show)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router Sitemap
sitemap =
     rHome
  <> rResource . ("static" </> anyString)

  <> rAddBook . ("book" </> "add")
  <> rBook . ("book" </> uuid)
  <> rEditBook . ("book" </> uuid . "edit")
  <> rAddEdition . ("book" </> uuid . "add-edition")

  <> rPerson . ("person" </> uuid)

  <> rEdition . ("edition" </> uuid)

  <> rPublisher . ("publisher" </> uuid)

  <> rSearch . "search"

  <> rLogin . "login"
  <> rRegister . "register"
  <> rLogout . "logout"

-- Note that this currently consumes the trailing / !
uuid :: PrinterParser StringsError [String] o ((BBID a) :- o)
uuid = xmaph (fromJust . parseBbid) (Just . show) anyString

--------------------------------------------------------------------------------
-- | Turn a 'Sitemap' value into a string URL with query parameters.
showURLParams :: Sitemap             -- ^ The path to convert into a URL.
              -> [(String, String)]  -- ^ An association list of query
                                     --   parameters.
              -> String
showURLParams url q = case unparseStrings sitemap url of
  Nothing -> error ("Could not route " ++ show url)
  Just ps -> ("/" ++) $ encodePathInfo ps q

--------------------------------------------------------------------------------
-- | Turn a 'Sitemap' value into a string, using no query parameters.
showURL :: Sitemap             -- ^ The path to convert into a URL.
        -> String
showURL u = showURLParams u []
