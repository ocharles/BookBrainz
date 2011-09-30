{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for the @/publisher@ resource.
module BookBrainz.Web.Handler.Publisher
       ( showPublisher
       ) where

import           BrainzStem.Model
import           BookBrainz.Types           (BBID)
import           BookBrainz.Model.Publisher ()
import           BookBrainz.Web.Handler  (output, onNothing)
import           BookBrainz.Web.Snaplet  (BookBrainzHandler)
import qualified BookBrainz.Web.View.Publisher as V

--------------------------------------------------------------------------------
{-| Show a single 'Publisher', searching by it's BBID. If the publisher cannot
be found, a 404 page is displayed. -}
showPublisher :: BBID -> BookBrainzHandler ()
showPublisher bbid = do
  publisher <- getByBbid bbid `onNothing` "Publisher not found"
  output $ V.showPublisher publisher
