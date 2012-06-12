{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.Handler.Search
       ( search
       ) where

import           Text.Digestive.Types (Method(..))
import           Text.Digestive.Snap (runFormWith, defaultSnapFormConfig, method)

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
  (v, r) <- runFormWith (defaultSnapFormConfig { method = Just Post })
              "search" Forms.searchForm
  case r of
    Just submission -> do
      results <- S.searchBooks (Forms.query submission)
      output $ V.searchResults results
    Nothing -> output $ V.searchPortal v

