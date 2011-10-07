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
import           Text.Digestive.Forms.Html (FormEncType)

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

--------------------------------------------------------------------------------
-- | Show a form for adding publishers.
addPublisher :: (Html, FormEncType)  -- ^ The form 'Html' and the encoding of it.
             -> View
addPublisher (formHtml, enctype) = do
  pageLayout Nothing $ do
    H.h1 "Add Publisher"
    H.form ! A.method "POST" ! A.enctype (toValue enctype) $ do
      formHtml
      H.p $ H.input ! A.type_ "submit" ! A.value "Add Book"
