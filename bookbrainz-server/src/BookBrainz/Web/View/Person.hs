{-# LANGUAGE OverloadedStrings #-}

-- | Views for 'Person's.
module BookBrainz.Web.View.Person
       ( -- * Pages
         showPerson
       , addPerson
       ) where

import           Data.Copointed
import           Text.Blaze.Html5    (toHtml, (!), toValue, Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Blaze.Html5
import qualified Text.Digestive.View as Form

import           BookBrainz.Types
import           BookBrainz.Web.View (pageLayout, View)

--------------------------------------------------------------------------------
-- | Display a single 'Person'.
showPerson :: LoadedCoreEntity Person  -- ^ The 'Person' to display.
           -> View
showPerson person =
  pageLayout Nothing $ do
    let person' = copoint person
    H.h1 $ toHtml $ personName person'

--------------------------------------------------------------------------------
-- | Show a form for adding publishers.
addPerson :: Form.View Html -- ^ The form 'Html' and the encoding of it.
          -> View
addPerson v =
  pageLayout Nothing $ do
    H.h1 "Add Person"
    H.form ! A.method "POST" ! A.enctype (toValue $ Form.viewEncType v) $ do
      H.p $ do
        label "name" v "Name:"
        inputText "name" v
      H.p $ H.input ! A.type_ "submit" ! A.value "Add Person"
