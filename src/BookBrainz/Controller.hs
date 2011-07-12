module BookBrainz.Controller
       ( generic404
       , genericError
       , onNothing
       , output
       ) where

import BookBrainz.Types.MVC
import qualified BookBrainz.View as V
import Control.Monad.Error
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Snap.Types (modifyResponse,setResponseCode,writeText)
import Text.Blaze (Html)
import Text.Blaze.Renderer.Text (renderHtml)

output :: Html -> Controller ()
output = writeText . toStrict . renderHtml

generic404 :: Text -> Controller ()
generic404 = genericError 404

genericError :: Int -> Text -> Controller ()
genericError status message = do
  modifyResponse $ setResponseCode status
  output $ V.genericError message

onNothing act handler = act >>= maybe (throwError handler) return
