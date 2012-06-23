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
import           BookBrainz.Web.Sitemap      as Sitemap (Sitemap(..), showURL)
import           BookBrainz.Web.View  (pageLayout, detailTable, linkBook, linkEdition, linkPerson, linkPublisher, View)
import           BookBrainz.Web.View.Role

--------------------------------------------------------------------------------
-- | Given a list of search results, display them in a human readable
-- table.
searchResults :: SearchResults S.SearchableBook
              -- ^ The 'Book' search results.
              -> SearchResults S.SearchableEdition
              -- ^ The 'Edition' search results.
              -> SearchResults S.SearchablePerson
              -- ^ The 'Person' search results.
              -> SearchResults S.SearchablePublisher
              -- ^ The 'Publisher' search results.
              -> View
searchResults books editions persons publishers = pageLayout Nothing $ do
  H.h2 "Books"
  detailTable [("Score", ["score"])
              ,("Book", [])
              ,("People", [])
              ]
            $ formatBook `map` getResults books
  H.h2 "Editions"
  detailTable [("Score", ["score"])
              ,("Edition", [])
              ,("People", [])
              ]
            $ formatEdition `map` getResults editions
  H.h2 "People"
  detailTable [("Score", ["score"])
              ,("Name", [])
              ]
            $ formatPerson `map` getResults persons
  H.h2 "Publisher"
  detailTable [("Score", ["score"])
              ,("Name", [])
              ]
            $ formatPublisher `map` getResults publishers
  where formatResult r columns = [ toHtml (round $ score r :: Int) ] ++ columns
        formatBook r = formatResult r [ linkBook $ S.bookResult $ result r
                                      , roleList $ S.bookRoles $ result r
                                      ]
        formatEdition r = formatResult r [ linkEdition $ S.editionResult $ result r
                                         , roleList $ S.editionRoles $ result r
                                         ]
        formatPerson r = formatResult r [ linkPerson $ S.personResult $ result r
                                        ]
        formatPublisher r = formatResult r [ linkPublisher $ S.publisherResult $ result r
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
