{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for the @/person@ resource.
module BookBrainz.Web.Handler.Person
       ( showPerson
       , addPerson
       ) where

import           Data.ByteString.Char8      (pack)
import           Snap.Core
import Snap.Snaplet.Hdbc (withTransaction')
import           Text.Digestive.Blaze.Html5 (renderFormHtml)
import           Text.Digestive.Forms.Snap  (eitherSnapForm)

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
addPerson = do
  withUser $  \user -> do
    r <- eitherSnapForm Forms.addPerson "person"
    case r of
      Left form' -> output $ V.addPerson $ renderFormHtml form'
      Right submission -> do
        person <- withTransaction' $ create submission $ entityRef user
        redirect $ pack . ("/person/" ++) . show . bbid $ person
