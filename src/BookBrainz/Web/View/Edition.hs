-- | View's for 'Edition's.
module BookBrainz.Web.View.Edition
       ( -- * Components
         linkEdition
       ) where

import Data.Copointed
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import BookBrainz.Types
import BookBrainz.Web.Sitemap as Sitemap (Sitemap(..), showURL)

--------------------------------------------------------------------------------
-- | Link to an edition.
linkEdition :: LoadedCoreEntity Edition  {-^ The 'Edition' to link to. Must be a
                                         'LoadedCoreEntity' in order to have a
                                         GID. -}
            -> Html
linkEdition edition =
  let uri = showURL $ Sitemap.Edition (gid edition) in
  a ! href (toValue uri) $ toHtml $ (editionName . copoint) edition
