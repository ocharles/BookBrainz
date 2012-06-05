{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.Handler.Search
       ( search
       ) where

import           Text.Digestive.Blaze.Html5
import           Text.Digestive.Snap (runForm)

import qualified BookBrainz.Forms           as Forms
import qualified BookBrainz.Search          as S
import           BookBrainz.Search          ()
import           BookBrainz.Web.Handler     (BookBrainzHandler, output)
import qualified BookBrainz.Web.View.Search as V

--------------------------------------------------------------------------------
-- | Search for books.
-- TODO Should handle publishers, people, editions.
-- TODO The view stuff doesn't belong here.
search :: BookBrainzHandler ()
search = do
  (v, r) <- runForm "search" Forms.searchForm
  case r of
    Just submission -> do
      results <- S.searchBooks (Forms.query submission)
      output $ V.searchResults results
    Nothing -> output $ V.searchPortal v

