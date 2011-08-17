module BookBrainz.Web.View.Search where

import Data.Monoid         (mempty)

import Text.Blaze          (Html)

import BookBrainz.Search
import BookBrainz.Web.View (pageLayout)

searchResults :: Html
searchResults = pageLayout Nothing mempty
