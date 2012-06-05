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
      H.p $ do
        label "userName" v "Username:"
        inputText "userName" v
        errorList "userName" v
      H.p $ do
        label "password" (Forms.subView "password" v) "Password:"
        inputPassword "password" (Forms.subView "password" v)
        errorList "password" (Forms.subView "password" v)
      H.p $ do
        label "confirmPassword" (Forms.subView "password" v) "Password:"
        inputPassword "confirmPassword" (Forms.subView "password" v)
        errorList "confirmPassword" (Forms.subView "password" v)
      errorList "password" v
      H.p $ do
        label "email" v "Email Address:"
        inputText "email" v
        errorList "email" v
      H.p $ H.input ! A.type_ "submit" ! A.value "Register"
