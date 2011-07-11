{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.View
       ( generic404
       , pageLayout
       ) where

import Data.Text (Text)
import Text.Blaze.Html5

generic404 :: Text -> Html
generic404 message = do
  pageLayout $ do
    h1 "Where's that damn thing got to?"
    p $ toHtml message

pageLayout :: Html -> Html
pageLayout = docTypeHtml
