{-# LANGUAGE OverloadedStrings #-}

-- | Views for 'Edition's.
module BookBrainz.Web.View.Edition
       ( -- * Pages
         showEdition
       ) where

import           Control.Monad       (when)
import           Data.Maybe          (isJust, fromJust)
import           Data.Monoid         (mappend, mconcat, mempty)

import           Data.Copointed
import           Text.Blaze.Html5    (Html, toHtml)
import qualified Text.Blaze.Html5 as H

import           BookBrainz.Types
import           BookBrainz.Web.View (pageLayout, linkBook, linkEdition)

--------------------------------------------------------------------------------
-- | Display a single 'Edition'.
showEdition :: ( LoadedCoreEntity Edition
               , LoadedCoreEntity Book
               , Maybe (LoadedEntity EditionFormat)
               , Maybe (LoadedEntity Country)
               , Maybe (LoadedEntity Language)
               )
            -- ^ The 'Edition' to display, with all necessary metadata
            -> Html
showEdition (edition, book, format, country, language) =
  pageLayout $ do
    H.h1 $ do
      linkEdition edition
      let year = (editionYear . copoint) edition
      when (isJust year) $
        mconcat [" (", (toHtml . fromJust) year, ")"]
    H.h2 $ "A version of " `mappend` linkBook book
    H.dl $ do
      optionalEntity   "Format" editionFormatName format
      optionalEntity   "Country" countryName country
      optionalEntity   "Language" languageName language
      optionalProperty "ISBN" $ (editionIsbn . copoint) edition
      optionalProperty "Barcode" $ (editionBarcode . copoint) edition
    where optionalEntity _ _ Nothing = mempty
          optionalEntity caption f (Just v) =
            optionalProperty caption $ Just $ (f . copoint) v
          optionalProperty _ Nothing = mempty
          optionalProperty caption (Just prop) = do
            H.dt caption
            H.dd $ toHtml prop
