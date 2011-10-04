{-# LANGUAGE OverloadedStrings #-}

-- | Views for 'Edition's.
module BookBrainz.Web.View.Edition
       ( -- * Pages
         showEdition
       ) where

import           Control.Monad       (when)
import           Data.Maybe          (isJust, fromJust)
import           Data.Monoid         (mappend, mconcat)

import           Data.Copointed
import           Text.Blaze.Html5    (toHtml)
import qualified Text.Blaze.Html5 as H

import           BookBrainz.Types
import           BookBrainz.Web.View (pageLayout, linkBook, linkEdition
                                     ,linkPublisher, optionalDl, View)
import qualified BookBrainz.Web.View.Sidebar as Sidebar

--------------------------------------------------------------------------------
-- | Display a single 'Edition'.
showEdition :: ( LoadedCoreEntity Edition
               , LoadedCoreEntity Book
               , Maybe (LoadedEntity EditionFormat)
               , Maybe (LoadedEntity Country)
               , Maybe (LoadedEntity Language)
               , Maybe (LoadedCoreEntity Publisher)
               , [(LoadedEntity Role, LoadedCoreEntity Person)]
               )
            -- ^ The 'Edition' to display, with all necessary metadata
            -> View
showEdition (edition, book, format, country, language, publisher, roles) =
  pageLayout (Just sidebar) $ do
    H.h1 $ do
      linkEdition edition
      let year = (editionYear . copoint) edition
      when (isJust year) $
        mconcat [" (", (toHtml . fromJust) year, ")"]
    H.h2 $ "A version of " `mappend` linkBook book
  where sidebar = do
          H.h2 "Edition information"
          optionalDl
            [("Format:",    fmap (toHtml.editionFormatName.copoint) format)
            ,("Country:",   fmap (toHtml.countryName.copoint) country)
            ,("Language:",  fmap (toHtml.languageName.copoint) language)
            ,("ISBN:",      fmap toHtml (editionIsbn $ copoint edition))
            ,("Publisher:", fmap linkPublisher publisher)
            ]
          Sidebar.roles roles
