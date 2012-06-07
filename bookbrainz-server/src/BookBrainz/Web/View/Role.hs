{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module BookBrainz.Web.View.Role where

import           Data.Copointed              (copoint)
import           Data.Monoid                 (mconcat)
import qualified Text.Digestive.View as Form
import           Text.Blaze.Html5          (toHtml, toValue, Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Blaze.Html5

import           BookBrainz.Web.View       (View, pageLayout, linkPerson)
import           BookBrainz.Web.View.Forms
import           BookBrainz.Web.Sitemap as Sitemap (Sitemap(..), showURL)
import           BookBrainz.Types

addRole :: Form.View Html  -- ^ The form 'Html' and the encoding of it.
        -> View
addRole v =
  pageLayout Nothing $ do
    H.h1 "Add Role"
    H.form ! A.method "POST" ! A.enctype (H.toValue $ Form.viewEncType v) $ do
      fieldTable v
        [ ("person", "Person:", inputPerson)
        , ("role", "Role:", inputSelect)
        ]
      submitRow "Add Role"
  where inputPerson n v' = inputSelect n v' >> " " >> addPerson
        addPerson = let uri = showURL $ Sitemap.AddPerson in
                    H.a ! A.href (toValue uri) $ "Add a new person"

roleList :: [LoadedEntity Role :. LoadedCoreEntity Person]
         -> Html
roleList roles = H.ul (formatRole `mapM_` roles)
  where formatRole (r :. p) = H.li $ do
          linkPerson p
          mconcat [" (", toHtml (roleName (copoint r)), ")"]
