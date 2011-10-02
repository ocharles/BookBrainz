{-# LANGUAGE OverloadedStrings #-}

-- | View's for 'Publisher's.
module BookBrainz.Web.View.Publisher
       ( -- * Pages
         showPublisher
       ) where

import Data.Copointed
import Text.Blaze.Html5    (toHtml)
import qualified Text.Blaze.Html5 as H

import BookBrainz.Types
import BookBrainz.Web.View (pageLayout, View)

--------------------------------------------------------------------------------
-- | Display a single 'Publisher'.
showPublisher :: LoadedCoreEntity Publisher  -- ^ The 'Publisher' to display
              -> View
showPublisher publisher =
  pageLayout Nothing $ do
    let publisher' = copoint publisher
    H.h1 $ toHtml $ publisherName publisher'
