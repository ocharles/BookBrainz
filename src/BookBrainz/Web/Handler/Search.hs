module BookBrainz.Web.Handler.Search
       ( search
       ) where

import           BookBrainz.Search      ()
import           BookBrainz.Web.Handler (BookBrainzHandler, output)
import qualified BookBrainz.Web.View.Search as V

search :: BookBrainzHandler ()
search = do
  output $ V.searchResults
