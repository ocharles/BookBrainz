{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.View.Book
       ( pageLayout
       , showBook
       ) where

import BookBrainz.Types
import BookBrainz.View (pageLayout)
import BookBrainz.View.AuthorCredit (linkAuthorCredit)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, Html, text)
import qualified Data.Text as T

showBook :: Book -> Html
showBook book =
  pageLayout $ do
    H.h1 $ toHtml $ bookName book
    H.h2 $ do
      H.span $ H.text "~"
      H.text " "
      linkAuthorCredit $ bookAuthorCredit book
