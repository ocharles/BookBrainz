{-# LANGUAGE OverloadedStrings #-}

-- | View's for 'Publisher's.
module BookBrainz.Web.View.Publisher
       ( -- * Pages
         addPublisher
       , showPublisher
       ) where

import Data.Copointed
import Text.Blaze.Html5    (toHtml, (!), toValue, Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive.View as Form

import BookBrainz.Types
import BookBrainz.Web.View.Edition (editionTable)
import BookBrainz.Web.View (pageLayout, View)
import BookBrainz.Web.View.Forms

--------------------------------------------------------------------------------
-- | Display a single 'Publisher'.
showPublisher :: LoadedCoreEntity Publisher  -- ^ The 'Publisher' to display
              -> [(LoadedCoreEntity Edition, Maybe (LoadedCoreEntity Publisher))]
              -- ^ All 'Edition's this 'Publisher' has published.
              -> View
showPublisher publisher editions =
  pageLayout Nothing $ do
    let publisher' = copoint publisher
    H.h1 $ toHtml $ publisherName publisher'
    H.h3 "Published Editions"
    editionTable editions

--------------------------------------------------------------------------------
-- | Show a form for adding publishers.
addPublisher :: Form.View Html  -- ^ The form 'Html' and the encoding of it.
             -> View
addPublisher v =
  pageLayout Nothing $ do
    H.h1 "Add Publisher"
    publisherForm v

publisherForm :: Form.View Html -> Html
publisherForm v =
  H.form ! A.method "POST" ! A.enctype (toValue $ Form.viewEncType v) $
    submitRow "Add Publisher"
