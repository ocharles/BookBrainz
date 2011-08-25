{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for the @/person@ resource.
module BookBrainz.Web.Handler.Person
       ( showPerson
       ) where

import           Data.UUID               (UUID)

import           BrainzStem.Model
import           BookBrainz.Model.Person ()
import           BookBrainz.Web.Handler  (output, onNothing)
import           BookBrainz.Web.Snaplet  (BookBrainzHandler)
import qualified BookBrainz.Web.View.Person as V

--------------------------------------------------------------------------------
{-| Show a single 'Person', searching by its GID. If the person cannot be found,
a 404 page is displayed. -}
showPerson :: UUID -> BookBrainzHandler ()
showPerson bbid = do
  person <- getByGid bbid `onNothing` "Person not found"
  output $ V.showPerson person
