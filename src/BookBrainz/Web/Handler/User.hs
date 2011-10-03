{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.Handler.User where

import           Data.Text.Encoding         as E
import           Snap.Core                  (redirect, getParam)
import           Snap.Snaplet               (with)
import           Snap.Snaplet.Auth          (Password (..), loginByUsername
                                            ,createUser, forceLogin)
import qualified Snap.Snaplet.Auth as Auth
import           Text.Digestive.Blaze.Html5 (renderFormHtml)
import           Text.Digestive.Forms.Snap  (eitherSnapForm)

import           BookBrainz.Forms           (Login (..), loginForm
                                            ,Registration (..), registerForm)
import           BookBrainz.Web.Handler
import           BookBrainz.Web.Snaplet
import qualified BookBrainz.Web.View.User   as V

login :: BookBrainzHandler ()
login = do
  r <- eitherSnapForm loginForm "login"
  case r of
    Left form' -> output $ V.login $ renderFormHtml form'
    Right submission -> do
      loginResult <- with auth $
        loginByUsername (E.encodeUtf8 $ loginFormId submission)
                        (ClearText $ E.encodeUtf8 $ loginFormPassword submission)
                        (loginFormRemember submission)
      case loginResult of
        Right _ -> getParam "redirect" >>= redirect . maybe "/" id
        Left _ -> return ()
        
register :: BookBrainzHandler ()
register = do
  r <- eitherSnapForm registerForm "register"
  case r of
    Left form' -> output $ V.login $ renderFormHtml form'
    Right submission -> do
      with auth $ do
        newUser <- createUser (newUserName submission)
                              (encodeUtf8 $ newUserPassword submission)
        forceLogin newUser
      redirect "/"

logout :: BookBrainzHandler ()
logout = do
  with auth $ Auth.logout
  redirect "/"

