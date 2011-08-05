{-# LANGUAGE DeriveDataTypeable #-}
module BookBrainz.Web.Handler
       ( BookBrainz(..)
       , BookBrainzHandler
       , database
       , output
       , onNothing
       , runHandler
       ) where

import Control.Monad.CatchIO (Exception, throw, tryJust)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Typeable
import Snap.Types
import Text.Blaze (Html)
import Text.Blaze.Renderer.Text (renderHtml)

import BookBrainz.Web.Snaplet
import qualified BookBrainz.Web.View as V

data HttpError = Http404 Text
  deriving (Show, Typeable)

instance Exception HttpError

output :: Html -> BookBrainzHandler ()
output = writeText . toStrict . renderHtml

action `onNothing` msg = action >>= maybe (throw $ Http404 msg) return

runHandler :: BookBrainzHandler () -> BookBrainzHandler ()
runHandler a = do
  outcome <- tryJust errorH a
  case outcome of
    Right r -> return r
    Left h' -> h'
  where errorH (Http404 message) = Just $ do
          output $ V.genericError message
