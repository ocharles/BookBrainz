module BookBrainz.View.Edition
       ( linkEdition
       ) where

import BookBrainz.Types
import Data.UUID (toString)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

linkEdition :: Edition -> Html
linkEdition edition = a ! href (uri edition) $ toHtml $ editionName edition
  where uri = toValue . ("/edition/" ++) . toString . editionGid
