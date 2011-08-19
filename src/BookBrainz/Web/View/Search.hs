{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.View.Search where

import Search.ElasticSearch (Document, SearchResults, getResults)
import Text.Blaze          (Html)

import qualified BookBrainz.Search as S
import BookBrainz.Web.View (pageLayout, detailTable, linkBook)

searchResults :: SearchResults S.SearchableBook -> Html
searchResults results = pageLayout Nothing $
  detailTable [("Book", [])] $ formatResult `map` getResults results
  where formatResult r = [ linkBook $ S.bookResult r ]
