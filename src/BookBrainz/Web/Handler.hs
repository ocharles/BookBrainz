{-# LANGUAGE DeriveDataTypeable #-}

-- | BookBrainz handlers, which are executed on page requests by users
module BookBrainz.Web.Handler
       ( BookBrainz
       , BookBrainzHandler
       , HttpError(..)

         -- * Helper functions
       , output
       , onNothing
       ) where

import Control.Monad.CatchIO    (Exception, throw)
import Control.Monad.IO.Class
import Data.Text                (Text)
import Data.Text.Lazy           (toStrict)
import Data.Typeable
import Snap.Types
import Text.Blaze               (Html)
import Text.Blaze.Renderer.Text (renderHtml)

import BookBrainz.Web.Snaplet

{-| A HTTP client or server error, that can be thrown in order to stop
processing a 'BookBrainzHandler'. -}
data HttpError = Http404 Text
  deriving (Show, Typeable)

instance Exception HttpError

--------------------------------------------------------------------------------
-- | Output 'Html'.
output :: Html -- ^ The 'Html' to display.
       -> BookBrainzHandler ()
output = writeText . toStrict . renderHtml

--------------------------------------------------------------------------------
{-| Run a action with a 'Maybe' result, and if it returns 'Nothing' then a
'Http404' will be thrown with a specific message. This will terminate
execution of the current handler. -}
onNothing :: MonadIO m
          => m (Maybe b)  {-^ The action that could potentially return
                          'Nothing'. -}
          -> Text         -- ^ The message to display to users on this 404.
          -> m b
action `onNothing` msg = action >>= maybe (throw $ Http404 msg) return
