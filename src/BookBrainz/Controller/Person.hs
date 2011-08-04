{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Controller.Person where

import BookBrainz.Controller
import BookBrainz.Model
import BookBrainz.Model.Person
import BookBrainz.Types.MVC (Controller)
import qualified BookBrainz.View.Person as V
import Control.Applicative
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromJust)
import Data.UUID (toString, fromString, UUID)
import Snap.Types

personResource :: UUID -> Controller ()
personResource bbid = do
  person <- model (getPerson bbid) `onNothing` "Person not found"
  output $ V.showPerson person
