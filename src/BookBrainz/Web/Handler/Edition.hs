{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Handlers for the @/edition@ resource.
module BookBrainz.Web.Handler.Edition
       ( showEdition
       , addEdition
       , editEdition
       ) where

import           Control.Applicative            ((<*>), (<$>))
import           Data.Traversable               (traverse)

import           Data.ByteString.Char8      (pack)
import           Data.Copointed                 (copoint)
import           Snap.Core
import           Text.Digestive.Forms.Snap      (eitherSnapForm)
import           Text.Digestive.Blaze.Html5     (renderFormHtml)

import           BrainzStem.Database        (withTransaction)
import qualified BookBrainz.Forms as Forms
import           BrainzStem.Model
import           BookBrainz.Model.Book          ()
import           BookBrainz.Model.Country       ()
import           BookBrainz.Model.Edition       ()
import           BookBrainz.Model.EditionFormat ()
import           BookBrainz.Model.Language      ()
import           BookBrainz.Model.Publisher     ()
import           BookBrainz.Model.Role          (findRoles)
import           BookBrainz.Types
import           BookBrainz.Web.Handler         (output, onNothing, withUser)
import           BookBrainz.Web.Snaplet         (BookBrainzHandler)
import qualified BookBrainz.Web.View.Edition as V

--------------------------------------------------------------------------------
{-| Show a single 'Edition', searching by its BBID. If the edition cannot be
found, a 404 page is displayed. -}
showEdition :: BBID Edition -> BookBrainzHandler ()
showEdition bbid' = do
  edition <- getByBbid bbid' `onNothing` "Edition not found"
  output =<< V.showEdition
    <$> do (edition, , , , , , )
             <$> getByConcept (editionBook   . copoint $ edition)
             <*> traverse getByPk (editionFormat . copoint $ edition)
             <*> traverse getByPk (editionCountry . copoint $ edition)
             <*> traverse getByPk (editionLanguage . copoint $ edition)
             <*> traverse getByConcept (editionPublisher . copoint $ edition)
             <*> findRoles (coreEntityTree edition)

--------------------------------------------------------------------------------
-- | Allow adding a new edition to a book
addEdition :: BBID Book -> BookBrainzHandler ()
addEdition bookBbid = do
  withUser $ \user -> do
    book <- getByBbid bookBbid `onNothing` "Book not found"
    editionForm <- Forms.addEdition $ coreEntityConcept book
    r <- eitherSnapForm editionForm "edition"
    case r of
      Left form' -> output $ V.addEdition $ renderFormHtml form'
      Right submission -> do
        edition <- withTransaction $
          create submission $ entityRef user
        redirect $ pack . ("/edition/" ++) . show . bbid $ edition

--------------------------------------------------------------------------------
-- | Allow editing an existing 'Edition'.
editEdition :: BBID Edition -> BookBrainzHandler ()
editEdition editionBbid = do
  withUser $ \user -> do
    edition <- getByBbid editionBbid `onNothing` "Edition not found"
    editionForm <- Forms.editEdition $ copoint edition
    r <- eitherSnapForm editionForm "edition"
    case r of
      Left form' -> output $ V.editEdition $ renderFormHtml form'
      Right submission -> do
        withTransaction $ do
          master <- findMasterBranch $ coreEntityConcept edition
          update master submission $ entityRef user
        redirect $ pack . ("/edition/" ++) . show . bbid $ edition
