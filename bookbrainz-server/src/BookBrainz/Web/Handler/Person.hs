{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for the @/person@ resource.
module BookBrainz.Web.Handler.Person
       ( showPerson
       , addPerson
       ) where

import           Data.ByteString.Char8      (pack)
import           Snap.Core
import Snap.Snaplet.PostgresqlSimple (withTransaction)
import           Text.Digestive.Snap  (runForm)

import qualified BookBrainz.Forms as Forms
import           BookBrainz.Model.Person    ()
import           BookBrainz.Types
import           BookBrainz.Web.Handler     (output, onNothing, withUser)
import           BookBrainz.Web.Snaplet     (BookBrainzHandler)
import qualified BookBrainz.Web.View.Person as V
import           BrainzStem.Model

--------------------------------------------------------------------------------
{-| Show a single 'Person', searching by its BBID. If the person cannot be found,
a 404 page is displayed. -}
showPerson :: BBID Person -> BookBrainzHandler ()
showPerson bbid' = do
  person <- getByBbid bbid' `onNothing` "Person not found"
  output $ V.showPerson person

--------------------------------------------------------------------------------
-- | Display a form for adding new 'Person's, add on submission add the
-- person.
addPerson :: BookBrainzHandler ()
addPerson =
  withUser $  \user -> do
    (v, r) <- runForm "person" Forms.addPerson
    case r of
      Nothing -> output $ V.addPerson v
      Just submission -> do
        person <- withTransaction $ create submission $ entityRef user
        redirect $ pack . ("/person/" ++) . show . bbid $ person
