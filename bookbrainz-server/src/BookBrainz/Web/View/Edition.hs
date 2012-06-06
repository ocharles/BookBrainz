{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Views for 'Edition's.
module BookBrainz.Web.View.Edition
       ( -- * Pages
         showEdition
       , addEdition
       , editEdition

         -- * Components
       , editionTable
       ) where

import           Control.Monad       (when)
import           Data.Copointed
import           Data.Maybe          (isJust, fromJust)
import           Data.Monoid         (mappend, mconcat)
import           Data.Text (Text)
import           Text.Blaze.Html5    (toHtml, (!), Html, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Blaze.Html5
import qualified Text.Digestive.View as Form

import           BookBrainz.Web.Sitemap    (Sitemap(..), showURL)
import           BookBrainz.Types
import           BookBrainz.Web.View (pageLayout, linkBook, linkEdition
                                     ,linkPublisher, optionalDl, View
                                     ,detailTable)
import           BookBrainz.Web.View.Forms
import qualified BookBrainz.Web.View.Sidebar as Sidebar
import           BookBrainz.Web.Sitemap as Sitemap (Sitemap(..), showURL)

--------------------------------------------------------------------------------
-- | Display a single 'Edition'.
showEdition :: ( LoadedCoreEntity Edition
               , LoadedCoreEntity Book
               , Maybe (LoadedEntity EditionFormat)
               , Maybe (LoadedEntity Country)
               , Maybe (LoadedEntity Language)
               , Maybe (LoadedCoreEntity Publisher)
               , [LoadedEntity Role :. LoadedCoreEntity Person]
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
    H.p $
      H.ul $ do
        H.li $ H.a ! A.href (toValue . showURL $ EditEdition $ bbid edition) $
                 "Edit this edition"
        H.li $ H.a ! A.href (toValue . showURL $ AddEditionRole $ bbid edition) $
                 "Add a new person-role"
  where sidebar = do
          H.h2 "Edition information"
          optionalDl
            [("Format:",    fmap (toHtml.editionFormatName.copoint) format)
            ,("Country:",   fmap (toHtml.countryName.copoint) country)
            ,("Language:",  fmap (toHtml.languageName.copoint) language)
            ,("ISBN:",      fmap (toHtml.show) (editionIsbn $ copoint edition))
            ,("Publisher:", fmap linkPublisher publisher)
            ]
          Sidebar.roles roles

--------------------------------------------------------------------------------
-- | A form for adding new 'Edition's.
addEdition :: Form.View Html  -- ^ The form 'Html' and the encoding of it.
           -> View
addEdition v =
  pageLayout Nothing $ do
    H.h1 "Add Edition"
    editionForm v "Add Edition"

--------------------------------------------------------------------------------
-- | A form for editing existing 'Edition's.
editEdition :: Form.View Html -- ^ The form 'Html' and the encoding of it.
            -> View
editEdition v =
  pageLayout Nothing $ do
    H.h1 "Edit Edition"
    editionForm v "Edit Edition"

--------------------------------------------------------------------------------
-- | A form to edit an 'Edition'.
editionForm :: Form.View Html -> Text -> Html
editionForm v submitLabel =
  H.form ! A.method "POST" ! A.enctype (H.toValue $ Form.viewEncType v) $ do
    fieldTable v
      [ ("name", "Name", inputText)
      , ("format", "Format:", inputSelect)
      , ("year", "Year:", inputYear)
      , ("publisher", "Publisher:", inputPublisher)
      , ("country", "Country:", inputSelect)
      , ("isbn", "ISBN:", inputIsbn)
      ]
    submitRow submitLabel
  where inputYear n v = inputText n v ! A.size "4"
        inputIsbn n v = inputText n v ! A.size "13"
        inputPublisher n v = inputSelect n v >> " " >> addPublisher
        addPublisher = let uri = showURL $ Sitemap.AddPublisher in
                       H.a ! A.href (toValue uri) $ "Add a new publisher"

--------------------------------------------------------------------------------
-- | Display a table of 'Edition's.
editionTable :: [(LoadedCoreEntity Edition, Maybe (LoadedCoreEntity Publisher))]
             -> Html
editionTable =
  detailTable [("Name", [])
              ,("Year", [])
              ,("ISBN", [])
              ,("Publisher",[])]
    . map editionRow
  where
    editionRow (edition, publisher) =
      let edition' = copoint edition in
      [ toHtml $ linkEdition edition
      , maybeCell toHtml $ editionYear edition'
      , maybeCell (toHtml . show) $ editionIsbn edition'
      , maybeCell linkPublisher publisher
      ]
    maybeCell f = toHtml . maybe "-" f
