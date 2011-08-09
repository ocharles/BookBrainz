{-# LANGUAGE OverloadedStrings #-}

-- | Generic views, common to many pages.
module BookBrainz.Web.View
       ( genericError
       , pageLayout
       ) where

import Data.Text (Text)
import Text.Blaze.Html5

--------------------------------------------------------------------------------
-- | Display a generic error page.
genericError :: Text -> Html
genericError message =
  pageLayout $ do
    h1 "Oops!"
    p $ toHtml message

--------------------------------------------------------------------------------
{-| The common layout for most BookBrainz pages, complete with heading, footer,
navigation links, etc. -}
pageLayout :: Html -> Html
pageLayout = docTypeHtml
