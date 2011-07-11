{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.View.Book
       ( showBook
       ) where

import BookBrainz.Types
import Text.Blaze.Html5

showBook :: Book -> Html
showBook book =
  docTypeHtml $ h1 $ toHtml $ bookName book
