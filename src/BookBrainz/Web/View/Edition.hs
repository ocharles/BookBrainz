-- | View's for 'Edition's.
module BookBrainz.Web.View.Edition
       ( -- * Components
         linkEdition
       ) where

import Data.Copointed
import Data.UUID (toString)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import BookBrainz.Types

--------------------------------------------------------------------------------
-- | Link to an edition.
linkEdition :: LoadedCoreEntity Edition  {-^ The 'Edition' to link to. Must be a
                                         'LoadedCoreEntity' in order to have a
                                         GID. -}
            -> Html
linkEdition edition = a ! href (uri edition) $
                      toHtml $ (editionName . copoint) edition
    where uri = toValue . ("/edition/" ++) . toString . gid
