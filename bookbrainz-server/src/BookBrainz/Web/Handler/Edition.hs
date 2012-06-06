{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Handlers for the @/edition@ resource.
module BookBrainz.Web.Handler.Edition
       ( showEdition
       , addEdition
       , editEdition
       , addEditionRole
       ) where

import           Control.Applicative            ((<*>), (<$>))
import           Data.Traversable               (traverse)

import           Data.ByteString.Char8      (pack)
import           Data.Copointed                 (copoint)
import           Snap.Core
import           Text.Digestive.Snap      (runForm)
import Snap.Snaplet.PostgresqlSimple (withTransaction)

import qualified BookBrainz.Forms as Forms
import           BrainzStem.Model
import           BookBrainz.Model.Book          ()
import           BookBrainz.Model.Country       ()
import           BookBrainz.Model.Edition       ()
import           BookBrainz.Model.EditionFormat ()
import           BookBrainz.Model.Language      ()
import           BookBrainz.Model.Publisher     ()
import           BookBrainz.Model.Role          (findRoles, addRole)
import           BookBrainz.Types
import           BookBrainz.Web.Handler         (output, onNothing, withUser)
import           BookBrainz.Web.Snaplet         (BookBrainzHandler)
import qualified BookBrainz.Web.View.Edition as V
import qualified BookBrainz.Web.View.Role as V

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
addEdition bookBbid =
  withUser $ \user -> do
    book <- getByBbid bookBbid `onNothing` "Book not found"
    editionForm <- Forms.addEdition $ coreEntityConcept book
    (v, r) <- runForm "edition" editionForm
    case r of
      Nothing -> output $ V.addEdition v
      Just submission -> do
        edition <- withTransaction $
          create submission $ entityRef user
        redirect $ pack . ("/edition/" ++) . show . bbid $ edition

--------------------------------------------------------------------------------
-- | Allow editing an existing 'Edition'.
editEdition :: BBID Edition -> BookBrainzHandler ()
editEdition editionBbid =
  withUser $ \user -> do
    edition <- getByBbid editionBbid `onNothing` "Edition not found"
    editionForm <- Forms.editEdition $ copoint edition
    (v, r) <- runForm "edition" editionForm
    case r of
      Nothing -> output $ V.editEdition v
      Just submission -> do
        withTransaction $ do
          master <- findMasterBranch $ coreEntityConcept edition
          changeBranch master (entityRef user) $ update submission
        redirect $ pack . ("/edition/" ++) . show . bbid $ edition

--------------------------------------------------------------------------------
{-| Present an interface for adding a new role to this edition. -}
addEditionRole :: BBID Edition -> BookBrainzHandler ()
addEditionRole bbid' =
  withUser $ \user -> do
    edition <- getByBbid bbid' `onNothing` "Edition not found"
    roleForm <- Forms.personRole
    (v, r) <- runForm "edition" roleForm
    case r of
      Nothing -> output $ V.addRole v
      Just submission -> do
        withTransaction $ do
          master <- findMasterBranch $ coreEntityConcept edition
          changeBranch master (entityRef user) $
            addRole submission
        redirect $ pack . ("/edition/" ++) . show . bbid $ edition
