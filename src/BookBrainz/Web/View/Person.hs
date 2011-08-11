{-# LANGUAGE OverloadedStrings #-}

-- | Views for 'Person's.
module BookBrainz.Web.View.Person
       ( -- * Pages
         showPerson
       ) where

import Data.Copointed
import Text.Blaze.Html5    (toHtml, Html)
import qualified Text.Blaze.Html5 as H

import BookBrainz.Types
import BookBrainz.Web.View (pageLayout)

--------------------------------------------------------------------------------
-- | Display a single 'Person'.
showPerson :: LoadedCoreEntity Person  -- ^ The 'Person' to display.
           -> Html
showPerson person =
  pageLayout $ do
    let person' = copoint person
    H.h1 $ toHtml $ personName person'
