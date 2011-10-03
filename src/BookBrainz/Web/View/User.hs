{-# LANGUAGE OverloadedStrings #-}

-- | Views for users.
module BookBrainz.Web.View.User
       ( -- * Pages
         login
       , register
       ) where

import           Control.Monad (when)
       
import           Text.Blaze.Html5            (Html, (!), toValue, toHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Forms.Html   (FormEncType)

import           BookBrainz.Web.View         (pageLayout, View)

--------------------------------------------------------------------------------
-- | A form authenticating.
login :: (Html, FormEncType)  -- ^ The form 'Html' and the encoding of it.
      -> View
login (formHtml, enctype) =
  pageLayout Nothing $ do
    H.h1 "Login"
    H.form ! A.method "POST" ! A.enctype (toValue enctype) $ do
      formHtml
      H.p $ H.input ! A.type_ "submit" ! A.value "Login"

--------------------------------------------------------------------------------
-- | A form for registering.
register :: (Html, FormEncType)  -- ^ The form 'Html' and the encoding of it.
         -> [String]
         -> View
register (formHtml, enctype) generalErrors =
  pageLayout Nothing $ do
    H.h1 "Become a BookBrainz Editor"
    H.form ! A.method "POST" ! A.enctype (toValue enctype) $ do
      when (not $ null generalErrors) $
        H.ul $ (H.li . toHtml) `mapM_` generalErrors
      formHtml
      H.p $ H.input ! A.type_ "submit" ! A.value "Register"
