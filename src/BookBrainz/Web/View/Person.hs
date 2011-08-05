{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.Web.View.Person
       ( showPerson
       ) where

import Data.Copointed
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, Html)

import BookBrainz.Types
import BookBrainz.Web.View (pageLayout)

showPerson :: LoadedCoreEntity Person -> Html
showPerson person =
  pageLayout $ do
    let person' = copoint person
    H.h1 $ toHtml $ personName person'
