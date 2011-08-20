{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.View.Search where

import           Data.Monoid          (mconcat)

import           Data.Copointed       (copoint)
import           Search.ElasticSearch (SearchResults(..)
                                      ,SearchResult(..))
import           Text.Blaze           (Html, toHtml)
import qualified Text.Blaze.Html5 as H

import qualified BookBrainz.Search as S
import           BookBrainz.Types
import           BookBrainz.Web.View  (pageLayout, detailTable, linkBook
                                      ,linkPerson)

searchResults :: SearchResults S.SearchableBook -> Html
searchResults results = pageLayout Nothing $
  detailTable [("Score", ["score"])
              ,("Book", [])
              ,("People", [])
              ]
            $ formatResult `map` getResults results
  where formatResult r = [ toHtml $ score r
                         , linkBook $ S.bookResult $ result r
                         , roleList $ S.bookRoles $ result r
                         ]

roleList :: [(LoadedEntity Role, LoadedCoreEntity Person)]
         -> Html
roleList roles = H.ul (formatRole `mapM_` roles)
  where formatRole (r, p) = H.li $ do
          linkPerson p
          mconcat [" (", toHtml (roleName (copoint r)), ")"]
