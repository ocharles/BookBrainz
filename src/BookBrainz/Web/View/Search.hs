{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.View.Search where

import           Data.Monoid                 (mconcat)

import           Data.Copointed              (copoint)
import           Search.ElasticSearch        (SearchResults(..)
                                             ,SearchResult(..))
import           Text.Blaze                  (Html, toHtml, (!), toValue)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Forms.Html   (FormEncType)

import qualified BookBrainz.Search           as S
import           BookBrainz.Types
import           BookBrainz.Web.Sitemap      as Sitemap (Sitemap(..), showURL)
import           BookBrainz.Web.View  (pageLayout, detailTable, linkBook
                                      ,linkPerson)

--------------------------------------------------------------------------------
-- | Given a list of search results, display them in a human readable
-- table.
searchResults :: SearchResults S.SearchableBook
              -- ^ The search results.
              -> Html
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

-- TODO Might be useful elsewhere, so this might end up in View.Role
roleList :: [(LoadedEntity Role, LoadedCoreEntity Person)]
         -> Html
roleList roles = H.ul (formatRole `mapM_` roles)
  where formatRole (r, p) = H.li $ do
          linkPerson p
          mconcat [" (", toHtml (roleName (copoint r)), ")"]

--------------------------------------------------------------------------------
-- | A form for beginning a search
searchPortal :: (Html, FormEncType)  -- ^ The form 'Html' and the encoding of it.
             -> Html
searchPortal (formHtml, enctype) =
  pageLayout Nothing $ do
    H.h1 "Search"
    H.form ! A.method "GET" ! A.action (toValue $ showURL Search) $ do
      formHtml
      H.p $ H.input ! A.type_ "submit" ! A.value "Search"
