{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.Web.View
       ( genericError
       , pageLayout
       ) where

import Data.Text (Text)
import Text.Blaze.Html5

genericError :: Text -> Html
genericError message =
  pageLayout $ do
    h1 "Oops!"
    p $ toHtml message

pageLayout :: Html -> Html
pageLayout = docTypeHtml