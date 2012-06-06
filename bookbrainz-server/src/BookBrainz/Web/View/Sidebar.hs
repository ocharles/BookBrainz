{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module BookBrainz.Web.View.Sidebar
       ( roles
       ) where

import           Data.Copointed      (copoint)
import           Text.Blaze.Html5    (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           BookBrainz.Types
import           BookBrainz.Web.View (linkPerson)

roles :: [LoadedEntity Role :. LoadedCoreEntity Person]
      -> Html
roles rs = do
  H.h2 "Roles"
  H.dl ! A.class_ "properties" $
    showRole `mapM_` rs
  where showRole (r :. p) = do
          H.dt $ (toHtml . roleName . copoint) r
          H.dl $ linkPerson p
