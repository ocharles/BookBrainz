{-# LANGUAGE OverloadedStrings #-}

-- | View's for 'Publisher's.
module BookBrainz.Web.View.Publisher
       ( -- * Pages
         showPublisher
       ) where

import Data.Copointed
import Text.Blaze.Html5    (toHtml, Html)
import qualified Text.Blaze.Html5 as H

import BookBrainz.Types
import BookBrainz.Web.View (pageLayout)

--------------------------------------------------------------------------------
-- | Display a single 'Publisher'.
showPublisher :: LoadedCoreEntity Publisher  -- ^ The 'Publisher' to display
           -> Html
showPublisher publisher =
  pageLayout Nothing $ do
    let publisher' = copoint publisher
    H.h1 $ toHtml $ publisherName publisher'
