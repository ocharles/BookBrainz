{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Handlers for the @/edition@ resource.
module BookBrainz.Web.Handler.Edition
       ( showEdition
       ) where

import           Control.Applicative            ((<*>), (<$>))
import           Data.Traversable               (traverse)

import           Data.Copointed                 (copoint)
import           Data.UUID

import           BrainzStem.Model
import           BookBrainz.Model.Book          ()
import           BookBrainz.Model.Country       ()
import           BookBrainz.Model.Edition       ()
import           BookBrainz.Model.EditionFormat ()
import           BookBrainz.Model.Language      ()
import           BookBrainz.Model.Publisher     ()
import           BookBrainz.Model.Role          (findRoles)
import           BookBrainz.Types               (editionBook, editionFormat
                                                ,editionCountry, editionLanguage
                                                ,editionPublisher, coreEntityTree)
import           BookBrainz.Web.Handler         (output, onNothing)
import           BookBrainz.Web.Snaplet         (BookBrainzHandler)
import qualified BookBrainz.Web.View.Edition as V

--------------------------------------------------------------------------------
{-| Show a single 'Edition', searching by its GID. If the edition cannot be
found, a 404 page is displayed. -}
showEdition :: UUID -> BookBrainzHandler ()
showEdition bbid = do
  edition <- getByGid bbid `onNothing` "Edition not found"
  output =<< V.showEdition
    <$> do (edition, , , , , , )
             <$> getByConcept (editionBook   . copoint $ edition)
             <*> traverse getByPk (editionFormat . copoint $ edition)
             <*> traverse getByPk (editionCountry . copoint $ edition)
             <*> traverse getByPk (editionLanguage . copoint $ edition)
             <*> traverse getByConcept (editionPublisher . copoint $ edition)
             <*> findRoles (coreEntityTree edition)
