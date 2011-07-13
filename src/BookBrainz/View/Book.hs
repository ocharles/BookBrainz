{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.View.Book
       ( pageLayout
       , showBook
       , showBooks
       ) where

import BookBrainz.Types
import BookBrainz.View (pageLayout)
import BookBrainz.View.AuthorCredit (linkAuthorCredit)
import BookBrainz.View.Edition (linkEdition)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, Html)

showBook :: Book -> [Edition] -> Html
showBook book editions =
  pageLayout $ do
    H.h1 $ toHtml $ bookName book
    H.h2 $ do
      H.span "~"
      " "
      linkAuthorCredit $ bookAuthorCredit book
    H.h3 "Editions"
    H.table $ do
      H.thead $
        H.tr $ do
          H.th "Name"
          H.th "Year"
      H.tbody $ editionRow `mapM_` editions
      where editionRow edition =
              H.tr $ do
                H.td $ toHtml $ linkEdition edition
                H.td $ toHtml $ maybe "-" show $ editionYear edition

showBooks :: [Book] -> Html
showBooks books =
  pageLayout $ do
    H.h1 "Books"
    H.ul $ bookLink `mapM_` books
    where bookLink book = H.li $ toHtml $ bookName book
