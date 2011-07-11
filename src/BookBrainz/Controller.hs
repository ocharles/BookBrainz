module BookBrainz.Controller
       ( output
       ) where

import BookBrainz.Types.MVC
import Snap.Types (writeText)
import Text.Blaze (Html)
import Text.Blaze.Renderer.Text (renderHtml)
import Data.Text.Lazy (toStrict)

output :: Html -> Controller ()
output = writeText . toStrict . renderHtml
