{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.Handler.Search
       ( search
       ) where

import           Text.Digestive.Blaze.Html5

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
  r <- Forms.processForm Forms.searchForm "search"
  case r of
    Right submission -> do
      results <- S.searchBooks (Forms.query submission)
      output $ V.searchResults results
    Left processedForm -> output $ V.searchPortal $ renderFormHtml processedForm

