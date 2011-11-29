{-# LANGUAGE DeriveDataTypeable #-}

-- | BookBrainz handlers, which are executed on page requests by users.
module BookBrainz.Web.Handler
       ( BookBrainz
       , BookBrainzHandler
       , HttpError(..)

         -- * Helper functions
       , output
       , onNothing
       , currentUser
       , withUser
       ) where

import Data.Maybe (fromJust)

import Database.HDBC            (toSql)
import Control.Monad.CatchIO    (Exception, throw)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.Text                (Text)
import Data.Text.Read           (decimal)
import Data.Typeable
import Snap.Blaze (blaze)
import Snap.Core
import Snap.Snaplet             (with)
import qualified Snap.Snaplet.Auth as SnapAuth
import Snap.Snaplet.Auth  (userLogin, userId, unUid)

import BookBrainz.Types
import BookBrainz.Web.Sitemap     (showURLParams, Sitemap(..))
import BookBrainz.Web.Snaplet
import BookBrainz.Web.View        (View, runView)

{-| A HTTP client or server error that can be thrown in order to stop
processing a 'BookBrainzHandler'. -}
data HttpError = Http404 Text
  deriving (Show, Typeable)

instance Exception HttpError

--------------------------------------------------------------------------------
-- | Output 'Html'.
output :: View -- ^ The 'View' to display.
       -> BookBrainzHandler ()
output view = do
  u <- currentUser
  blaze $ runView view u

--------------------------------------------------------------------------------
{-| Run an action with a 'Maybe' result, and if it returns 'Nothing', then a
'Http404' will be thrown with a specific message. This will terminate
execution of the current handler. -}
onNothing :: MonadIO m
          => m (Maybe b)  {-^ The action that could potentially return
                          'Nothing'. -}
          -> Text         -- ^ The message to display to users with this 404.
          -> m b
action `onNothing` msg = action >>= maybe (throw $ Http404 msg) return

--------------------------------------------------------------------------------
-- | Get the current user, as a BrainzStem specific 'Editor' type.
currentUser :: BookBrainzHandler (Maybe (LoadedEntity Editor))
currentUser = fmap auToBbE `fmap` (with auth $ SnapAuth.currentUser)
  where auToBbE user =
          Entity {
            entityInfo = Editor { editorName = userLogin user }
          , entityRef = Ref . toSql . uidResult . decimal . unUid . fromJust $
                          userId user
          }
        uidResult :: Either String (Int, Text) -> Int
        uidResult (Left _) = error "Could not read user ID"
        uidResult (Right (id', _)) = id'

--------------------------------------------------------------------------------
-- | If the user is not logged in, redirect them to the login page with a sane
-- redirection handler. Otherwise, return the currently logged in user.
withUser :: (LoadedEntity Editor -> BookBrainzHandler ()) -> BookBrainzHandler ()
withUser f = do
  u <- currentUser
  case u of
    Nothing -> do
      redir <- rqURI `fmap` getRequest
      redirect . BS.pack $ showURLParams Login [("redirect", BS.unpack redir)]
    Just user -> f user
