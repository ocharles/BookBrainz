{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.View.Person
       ( showPerson
       ) where

import BookBrainz.Types
import BookBrainz.View (pageLayout)
import Data.Copointed
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, Html)

showPerson :: LoadedCoreEntity Person -> Html
showPerson person =
  pageLayout $ do
    let person' = copoint person
    H.h1 $ toHtml $ personName person'
