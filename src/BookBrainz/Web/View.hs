{-# LANGUAGE OverloadedStrings #-}

-- | Generic views, common to many pages.
module BookBrainz.Web.View
       ( genericError
       , pageLayout

         -- * Linking
       , linkBook
       , linkEdition
       , linkPerson
       , linkPublisher

         -- * Components
       , optionalDl
       , detailTable
       ) where

import           Data.Monoid                       (mempty)

import           Data.Copointed
import           Data.Text                         (Text)
import           Text.Blaze.Html5                  (Html, toHtml, (!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           BookBrainz.Types
import           BookBrainz.Web.Sitemap as Sitemap (Sitemap(..), showURL)

--------------------------------------------------------------------------------
-- | Display a generic error page.
genericError :: Text -> Html
genericError message =
  pageLayout Nothing $ do
    H.h1 "Oops!"
    H.p $ toHtml message

--------------------------------------------------------------------------------
{-| The common layout for most BookBrainz pages, complete with heading, footer,
navigation links, etc. -}
pageLayout :: Maybe Html   -- ^ Optional 'Html' for the sidebar.
           -> Html         -- ^ The main content of the page.
           -> Html
pageLayout sidebar body = H.docTypeHtml $ do
  H.head $ do
    H.title "BookBrainz"
    H.link ! A.type_ "text/css" ! A.rel "stylesheet"
           ! (A.href . toValue . showURL $ Resource "style.css")
  H.body $ do
    H.div ! A.id "header" $ do
      H.div ! A.id "header-logo" $ do
        linkHome H.img
        H.div ! A.id "header-search" $ mempty -- TODO Search form
      H.div ! A.id "header-menu" $
        H.div $
          H.ul $
            H.li $ linkHome "BookBrainz" -- TODO Navigation menu
    H.div ! A.id "page" $ do
      showSidebar sidebar
      H.div ! A.id "content" $ body -- TODO Sidebar
    H.div ! A.id "footer" $ mempty -- TODO Footer links
  where linkHome = H.a ! A.href (toValue $ showURL Home)
        showSidebar (Just s) = H.div ! A.id "sidebar" $ s
        showSidebar _ = mempty

--------------------------------------------------------------------------------
-- | Link to a book.
linkBook :: LoadedCoreEntity Book  {-^ The 'Book' to link to. Must be a
                                   'LoadedCoreEntity' in order to have a
                                   GID. -}
         -> Html
linkBook book =
  let uri = showURL $ Sitemap.Book (gid book) in
  H.a ! A.href (toValue uri) $ toHtml $ bookName $ copoint book

--------------------------------------------------------------------------------
-- | Link to an edition.
linkEdition :: LoadedCoreEntity Edition  {-^ The 'Edition' to link to. Must be a
                                         'LoadedCoreEntity' in order to have a
                                         GID. -}
            -> Html
linkEdition edition =
  let uri = showURL $ Sitemap.Edition (gid edition) in
  H.a ! A.href (toValue uri) $ toHtml $ (editionName . copoint) edition

--------------------------------------------------------------------------------
-- | Link to a publisher.
linkPublisher :: LoadedCoreEntity Publisher
              -- ^ The 'Edition' to link to. Must be a 'LoadedCoreEntity' in
              -- order to have a GID.
              -> Html
linkPublisher p =
  let uri = showURL $ Sitemap.Publisher (gid p) in
  H.a ! A.href (toValue uri) $ toHtml $ publisherName $ copoint p

--------------------------------------------------------------------------------
-- | Link to a person.
linkPerson :: LoadedCoreEntity Person
           -- ^ The 'Person' to link to. Must be a 'LoadedCoreEntity' in
           -- order to have a GID.
           -> Html
linkPerson p =
  let uri = showURL $ Sitemap.Person (gid p) in
  H.a ! A.href (toValue uri) $ toHtml $ personName $ copoint p

--------------------------------------------------------------------------------
-- | Takes a list of sidebar captions, and possible values for them. If the
-- value is 'Nothing', then the sidebar property is ommited.
optionalDl :: [(Text, Maybe Html)] -> Html
optionalDl values = H.dl ! A.class_ "properties" $ showDef `mapM_` values
  where
    showDef (_, Nothing) = mempty
    showDef (caption, Just html) = do
      H.dt $ toHtml caption
      H.dd html

--------------------------------------------------------------------------------
-- | Displays a table of detailed information
detailTable :: [(Html, [String])] -- ^ A list of column names, and the class for
                                  -- that column.
            -> [[Html]]           -- ^ A list of rows. Each row is a list of cell
                                  -- values.
            -> Html
detailTable headers rows =
  H.table ! A.class_ "details" $ do
    H.thead $ H.tr $ header `mapM_` headers
    H.tbody $ mapEven_ (row "odd") (row "even") rows
  where header (caption, classes) =
          H.th ! A.class_ (toValue $ unwords classes) $ caption
        row c cells = H.tr ! A.class_ c $ H.td `mapM_` cells
        mapEven o e (x:y:xs) = [o x, e y] ++ mapEven o e xs
        mapEven o _ [x] = [o x]
        mapEven _ _ [] = []
        mapEven_ o e as = sequence_ (mapEven o e as)
