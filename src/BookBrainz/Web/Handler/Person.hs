{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for the @/person@ resource.
module BookBrainz.Web.Handler.Person
       ( showPerson
       ) where

import           Data.UUID (UUID)

import           BookBrainz.Model.Person
import           BookBrainz.Web.Handler (output, onNothing)
import           BookBrainz.Web.Snaplet (BookBrainzHandler)
import qualified BookBrainz.Web.View.Person as V

--------------------------------------------------------------------------------
{-| Show a single 'Book', searching by it's GID. If the book cannot be found,
a 404 page is displayed. -}
showPerson :: UUID -> BookBrainzHandler ()
showPerson bbid = do
  person <- getPerson bbid `onNothing` "Person not found"
  output $ V.showPerson person
