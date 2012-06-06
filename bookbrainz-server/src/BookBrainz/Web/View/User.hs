{-# LANGUAGE OverloadedStrings #-}

-- | Views for users.
module BookBrainz.Web.View.User
       ( -- * Pages
         login
       , register
       ) where

import           Text.Blaze.Html5            (Html, (!), toValue)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Blaze.Html5
import qualified Text.Digestive.View as Forms

import           BookBrainz.Web.View         (pageLayout, View)
import           BookBrainz.Web.View.Forms

--------------------------------------------------------------------------------
-- | A form authenticating.
login :: Forms.View Html -- ^ The form 'Html' and the encoding of it.
      -> View
login v =
  pageLayout Nothing $ do
    H.h1 "Login"
    H.form ! A.method "POST" ! A.enctype (toValue $ Forms.viewEncType v) $
      H.p $ H.input ! A.type_ "submit" ! A.value "Login"

--------------------------------------------------------------------------------
-- | A form for registering.
register :: Forms.View Html -- ^ The form 'Html' and the encoding of it.
         -> View
register v =
  pageLayout Nothing $ do
    H.h1 "Become a BookBrainz Editor"
    H.form ! A.method "POST" ! A.enctype (toValue $ Forms.viewEncType v) $ do
      fieldRow v "userName" "Username:" inputText
      fieldTable (Forms.subView "password" v)
        [ ("password", "Password:", inputPassword)
        , ("confirmPassword", "Confirm password:", inputPassword)
        ]
      fieldRow v "email" "Email Address:" inputText
      submitRow "Register"
