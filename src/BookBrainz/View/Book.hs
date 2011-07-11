{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.View.Book
       ( pageLayout
       , showBook
       ) where

import BookBrainz.Types
import BookBrainz.View (pageLayout)
import BookBrainz.View.AuthorCredit (linkAuthorCredit)
import BookBrainz.View.Edition (linkEdition)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, Html, text)
import qualified Data.Text as T

showBook :: Book -> [Edition] -> Html
showBook book editions =
  pageLayout $ do
    H.h1 $ toHtml $ bookName book
    H.h2 $ do
      H.span $ H.text "~"
      H.text " "
      linkAuthorCredit $ bookAuthorCredit book
    H.h3 "Editions"
    H.table $ do
      H.thead $ do
        H.tr $ do
          H.th "Name"
          H.th "Year"
      H.tbody $ editionRow `mapM_` editions
      where editionRow edition = do
              H.tr $ do
                H.td $ toHtml $ linkEdition edition
                H.td $ toHtml $ maybe "-" show $ editionYear edition
