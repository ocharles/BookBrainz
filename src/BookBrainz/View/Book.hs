{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.View.Book
       ( pageLayout
       , showBook
       ) where

import BookBrainz.Types
import BookBrainz.View (pageLayout)
import Text.Blaze.Html5

showBook :: Book -> Html
showBook book =
  pageLayout $ h1 $ toHtml $ bookName book
