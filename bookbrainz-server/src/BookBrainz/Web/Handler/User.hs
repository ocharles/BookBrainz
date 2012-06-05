{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.Handler.User where

import           Data.Text.Encoding         as E
import           Snap.Core                  (redirect, getParam)
import           Snap.Snaplet               (with)
import           Snap.Snaplet.Auth          (Password (..), loginByUsername
                                            ,createUser, forceLogin)
import qualified Snap.Snaplet.Auth as Auth
import           Text.Digestive.Snap  (runForm)

import           BookBrainz.Forms           (Login (..), loginForm
                                            ,Registration (..), registerForm)
import           BookBrainz.Web.Handler
import           BookBrainz.Web.Snaplet
import qualified BookBrainz.Web.View.User   as V

login :: BookBrainzHandler ()
login = do
  (v, r) <- runForm "login" loginForm
  case r of
    Nothing -> output $ V.login v
    Just submission -> do
      loginResult <- with auth $
        loginByUsername (E.encodeUtf8 $ loginFormId submission)
                        (ClearText $ E.encodeUtf8 $ loginFormPassword submission)
                        (loginFormRemember submission)
      case loginResult of
        Right _ -> getParam "redirect" >>= redirect . maybe "/" id
        Left _ -> return ()

register :: BookBrainzHandler ()
register = do
  (v, r) <- runForm "register" registerForm
  case r of
    Nothing -> output $ V.register v
    Just submission -> do
      with auth $ do
        newUser <- createUser (newUserName submission)
                              (encodeUtf8 $ newUserPassword submission)
        forceLogin newUser
      redirect "/"

logout :: BookBrainzHandler ()
logout = do
  with auth $ Auth.logout
  redirect "/"

