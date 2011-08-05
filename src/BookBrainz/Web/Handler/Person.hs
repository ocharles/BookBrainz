{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.Handler.Person
       ( showPerson
       ) where

import           Data.UUID (UUID)

import           BookBrainz.Model.Person
import           BookBrainz.Web.Handler (output, onNothing)
import           BookBrainz.Web.Snaplet (BookBrainzHandler)
import qualified BookBrainz.Web.View.Person as V

showPerson :: UUID -> BookBrainzHandler ()
showPerson bbid = do
  person <- getPerson bbid `onNothing` "Person not found"
  output $ V.showPerson person
