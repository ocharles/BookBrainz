module BookBrainz.Controller
       ( generic404
       , output
       ) where

import BookBrainz.Types.MVC
import qualified BookBrainz.View as V
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Snap.Types (modifyResponse,setResponseCode,writeText)
import Text.Blaze (Html)
import Text.Blaze.Renderer.Text (renderHtml)

output :: Html -> Controller ()
output = writeText . toStrict . renderHtml

generic404 :: Text -> Controller ()
generic404 message = do
  output $ V.generic404 message
  modifyResponse $ setResponseCode 404
