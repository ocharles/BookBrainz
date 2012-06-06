{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module BookBrainz.Web.View.Search where

import           Search.ElasticSearch        (SearchResults(..)
                                             ,SearchResult(..))
import           Text.Blaze.Html             (Html, toHtml, (!), toValue)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive.View as Forms

import qualified BookBrainz.Search           as S
import           BookBrainz.Types
import           BookBrainz.Web.Sitemap      as Sitemap (Sitemap(..), showURL)
import           BookBrainz.Web.View  (pageLayout, detailTable, linkBook
                                      ,linkPerson, View)
import           BookBrainz.Web.View.Role

--------------------------------------------------------------------------------
-- | Given a list of search results, display them in a human readable
-- table.
searchResults :: SearchResults S.SearchableBook
              -- ^ The search results.
              -> View
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

--------------------------------------------------------------------------------
-- | A form for beginning a search
searchPortal :: Forms.View Html -- ^ The form 'Html' and the encoding of it.
             -> View
searchPortal v =
  pageLayout Nothing $ do
    H.h1 "Search"
    H.form ! A.method "GET" ! A.action (toValue $ showURL Search) ! A.enctype (toValue $ Forms.viewEncType v) $
      H.p $ H.input ! A.type_ "submit" ! A.value "Search"
