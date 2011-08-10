{-# LANGUAGE OverloadedStrings #-}

-- | View's for 'Edition's.
module BookBrainz.Web.View.Edition
       ( -- * Pages
         showEdition
       ) where

import           Data.Monoid         (mappend)

import           Data.Copointed
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H

import           BookBrainz.Types
import           BookBrainz.Web.View (pageLayout, linkBook)

--------------------------------------------------------------------------------
-- | Display a single 'Edition'.
showEdition :: LoadedCoreEntity Edition  -- ^ The 'Edition' to display.
            -> LoadedCoreEntity Book     -- ^ The 'Book' this is an edition of.
            -> Html
showEdition edition book =
  pageLayout $ do
    let edition' = copoint edition
    H.h1 $ toHtml $ editionName edition'
    H.h2 $ "A version of " `mappend` linkBook book
