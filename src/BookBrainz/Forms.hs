{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module BookBrainz.Forms where

import           Control.Applicative         ((<$>), (<*>))
import           Data.Maybe                  (fromMaybe)

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Snap.Core
import           Text.Blaze.Html5            (Html, (!), toValue)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive
import           Text.Digestive.Blaze.Html5
import           Text.Digestive.Forms        (FormInput(..))
import qualified Text.Digestive.Forms        as Forms
import           Text.Digestive.Forms.Snap

import           BrainzStem.Database (HasDatabase)
import           BookBrainz.Model.Editor (getEditorByName)
import           BookBrainz.Types

data SearchQuery = SearchQuery { query :: Text }

data Login = Login { loginFormId :: Text
                   , loginFormPassword :: Text
                   , loginFormRemember :: Bool
                   }

data Registration = Registration { newUserName :: Text
                                 , newUserPassword :: Text
                                 , newUserPasswordConfirmation :: Text
                                 , newUserEmail :: Text
                                 }

--------------------------------------------------------------------------------
bookForm :: (Monad m, MonadSnap m)
         => Maybe Book
         -> SnapForm m Html BlazeFormHtml Book
bookForm book = Book <$> simpleField "Book title:"
                           (inputText (bookName `fmap` book) `validate` nonEmpty "Book title cannot be empty")

--------------------------------------------------------------------------------
searchForm :: (Monad m, MonadSnap m) => SnapForm m Html BlazeFormHtml SearchQuery
searchForm = SearchQuery <$> simpleField "Query: "
                               (inputText Nothing `validate` nonEmpty "Query cannot be empty")

--------------------------------------------------------------------------------
userName :: (Monad m, MonadSnap m)
         => Maybe Text -> SnapForm m Html BlazeFormHtml Text
userName inp = inputText inp `validate` nonEmpty "Please enter a username"

password :: (Monad m, MonadSnap m)
         => Maybe Text -> SnapForm m Html BlazeFormHtml Text
password inp = inputPass inp `validate` nonEmpty "Please enter a password"

registerForm :: (Monad m, MonadSnap m, HasDatabase m)
             => SnapForm m Html BlazeFormHtml Registration
registerForm = (`transform` newUserCheck) $ (`validate` passwordsMatch) $ (errors ++>) $
                  Registration <$> simpleField "User name:" (userName Nothing)
                               <*> simpleField "Password:" (password Nothing)
                               <*> simpleField "Confirm password:" (password Nothing)
                               <*> simpleField "Email address:"
                                     (inputText Nothing)
  where passwordsMatch = check "Passwords must match" $ \reg ->
          newUserPassword reg == newUserPasswordConfirmation reg
        newUserCheck = transformEitherM $ \submission -> do
          editor <- getEditorByName $ newUserName submission
          return $ case editor of
            Just _  -> Left "An account with this name already exists"
            Nothing -> Right submission

loginForm :: (Monad m, MonadSnap m)
          => SnapForm m Html BlazeFormHtml Login
loginForm = Login <$> simpleField "User name:"
                        (inputText Nothing `validate` nonEmpty "Please enter a username")
                  <*> simpleField "Password:"
                        (inputPass Nothing `validate` nonEmpty "Please enter a password")
                  <*> mapViewHtml H.p (inputCheckBox False <++ label "Remember me")

--------------------------------------------------------------------------------                  
nonEmpty :: Monad m => Html -> Validator m Html Text
nonEmpty message = check message (not . T.null)

--------------------------------------------------------------------------------
simpleField :: (Monad m, MonadSnap m)
            => String
            -> SnapForm m Html BlazeFormHtml a
            -> SnapForm m Html BlazeFormHtml a
simpleField labelText field = mapViewHtml H.p (label labelText ++> field <++ errors)

--------------------------------------------------------------------------------
inputPass :: (Monad m, Functor m, FormInput i f)
          => Formlet m i e BlazeFormHtml Text
inputPass = Forms.inputText $ \id' inp -> createFormHtml $ \_ ->
  H.input ! A.type_ "password"
          ! A.name (toValue $ show id')
          ! A.id (toValue $ show id')
          ! A.value (toValue $ fromMaybe "" inp)

--------------------------------------------------------------------------------          
processForm :: (MonadSnap m)
            => SnapForm m e v a  -- ^ Form
            -> String            -- ^ Form name
            -> m (Either v a)    -- ^ Result
processForm form name = eitherForm form name snapEnvironment
