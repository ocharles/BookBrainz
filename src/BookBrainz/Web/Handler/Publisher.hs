{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for the @/publisher@ resource.
module BookBrainz.Web.Handler.Publisher
       ( showPublisher
       ) where

import           Data.UUID               (UUID)

import           BrainzStem.Model
import           BookBrainz.Model.Publisher ()
import           BookBrainz.Web.Handler  (output, onNothing)
import           BookBrainz.Web.Snaplet  (BookBrainzHandler)
import qualified BookBrainz.Web.View.Publisher as V

--------------------------------------------------------------------------------
{-| Show a single 'Publisher', searching by it's GID. If the publisher cannot be 
found, a 404 page is displayed. -}
showPublisher :: UUID -> BookBrainzHandler ()
showPublisher bbid = do
  publisher <- getByGid bbid `onNothing` "Publisher not found"
  output $ V.showPublisher publisher
