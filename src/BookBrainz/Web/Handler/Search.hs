{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.Handler.Search
       ( search
       ) where

import qualified BookBrainz.Search as S
import           BookBrainz.Search      ()
import           BookBrainz.Web.Handler (BookBrainzHandler, output)
import qualified BookBrainz.Web.View.Search as V

search :: BookBrainzHandler ()
search = do
  results <- S.searchBooks "hitch"
  output $ V.searchResults results
