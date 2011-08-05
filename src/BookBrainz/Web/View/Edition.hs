module BookBrainz.Web.View.Edition
       ( linkEdition
       ) where

import BookBrainz.Types
import Data.Copointed
import Data.UUID (toString)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

linkEdition :: LoadedCoreEntity Edition -> Html
linkEdition edition = a ! href (uri edition) $ toHtml $ editionName $ copoint edition
  where uri = toValue . ("/edition/" ++) . toString . gid
