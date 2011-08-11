{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Handlers for the @/edition@ resource
module BookBrainz.Web.Handler.Edition
       ( showEdition
       ) where

import           Control.Applicative            ((<*>), (<$>))
import           Data.Traversable               (traverse)

import           Data.Copointed                 (copoint)
import           Data.UUID

import           BookBrainz.Model
import           BookBrainz.Model.Book          ()
import           BookBrainz.Model.Country       ()
import           BookBrainz.Model.Edition       ()
import           BookBrainz.Model.EditionFormat ()
import           BookBrainz.Model.Language      ()
import           BookBrainz.Types               (editionBook, editionFormat
                                                ,editionCountry, editionLanguage)
import           BookBrainz.Web.Handler         (output, onNothing)
import           BookBrainz.Web.Snaplet         (BookBrainzHandler)
import qualified BookBrainz.Web.View.Edition as V

--------------------------------------------------------------------------------
{-| Show a single 'Edition', searching by it's GID. If the edition cannot be
found, a 404 page is displayed. -}
showEdition :: UUID -> BookBrainzHandler ()
showEdition bbid = do
  edition <- getByGid bbid `onNothing` "Edition not found"
  output =<< V.showEdition
    <$> do (edition, , , , )
             <$> getVersion (editionBook   . copoint $ edition)
             <*> traverse getByKey (editionFormat . copoint $ edition)
             <*> traverse getByKey (editionCountry . copoint $ edition)
             <*> traverse getByKey (editionLanguage . copoint $ edition)
