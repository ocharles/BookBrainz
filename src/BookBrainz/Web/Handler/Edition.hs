{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for the @/edition@ resource
module BookBrainz.Web.Handler.Edition
       ( showEdition
       ) where

import           Data.Copointed           (copoint)
import           Data.UUID

import           BookBrainz.Model
import           BookBrainz.Model.Book    ()
import           BookBrainz.Model.Edition ()
import           BookBrainz.Types         (editionBook)
import           BookBrainz.Web.Handler   (output, onNothing)
import           BookBrainz.Web.Snaplet   (BookBrainzHandler)
import qualified BookBrainz.Web.View.Edition as V

--------------------------------------------------------------------------------
{-| Show a single 'Edition', searching by it's GID. If the edition cannot be
found, a 404 page is displayed. -}
showEdition :: UUID -> BookBrainzHandler ()
showEdition bbid = do
  edition <- getByGid bbid `onNothing` "Edition not found"
  book    <- getVersion $ editionBook . copoint $ edition
  output $ V.showEdition edition book
