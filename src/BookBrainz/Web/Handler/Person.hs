{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for the @/person@ resource.
module BookBrainz.Web.Handler.Person
       ( showPerson
       ) where

import           BookBrainz.Model.Person    ()
import           BookBrainz.Types           (BBID)
import           BookBrainz.Web.Handler     (output, onNothing)
import           BookBrainz.Web.Snaplet     (BookBrainzHandler)
import qualified BookBrainz.Web.View.Person as V
import           BrainzStem.Model

--------------------------------------------------------------------------------
{-| Show a single 'Person', searching by its BBID. If the person cannot be found,
a 404 page is displayed. -}
showPerson :: BBID -> BookBrainzHandler ()
showPerson bbid = do
  person <- getByBbid bbid `onNothing` "Person not found"
  output $ V.showPerson person
