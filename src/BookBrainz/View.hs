{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.View
       ( genericError
       , pageLayout
       ) where

import Data.Text (Text)
import Text.Blaze.Html5

genericError :: Text -> Html
genericError message = do
  pageLayout $ do
    h1 "Oops!"
    p $ toHtml message

pageLayout :: Html -> Html
pageLayout = docTypeHtml
