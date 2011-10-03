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
import           Text.Digestive.Result      (Result(Ok, Error))
import           Text.Digestive.Types       (unView)

import           BookBrainz.Forms           (Login (..), loginForm
                                            ,Registration (..), registerForm
                                            ,runForm)
import           BookBrainz.Model.Editor    (getEditorByName)
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
  (view', result) <- runForm registerForm "register"
  case result of
    Ok submission -> do
      editor <- getEditorByName $ newUserName submission
      case editor of
        Just _ ->
          output $ V.register (renderFormHtml $ unView view' [])
                              [ "This account already exists" ]
        Nothing -> do
          with auth $ do
            newUser <- createUser (newUserName submission)
                                  (encodeUtf8 $ newUserPassword submission)
            forceLogin newUser
          redirect "/"
    Error e -> output $ V.register (renderFormHtml $ unView view' e) []

logout :: BookBrainzHandler ()
logout = do
  with auth $ Auth.logout
  redirect "/"

