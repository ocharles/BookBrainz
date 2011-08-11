{-# LANGUAGE OverloadedStrings #-}

-- | Generic views, common to many pages.
module BookBrainz.Web.View
       ( genericError
       , pageLayout

         -- * Linking
       , linkBook
       , linkEdition
       , linkPublisher
       ) where

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
  pageLayout $ do
    H.h1 "Oops!"
    H.p $ toHtml message

--------------------------------------------------------------------------------
{-| The common layout for most BookBrainz pages, complete with heading, footer,
navigation links, etc. -}
pageLayout :: Html -> Html
pageLayout = H.docTypeHtml

--------------------------------------------------------------------------------
-- | Link to a book.
linkBook :: LoadedCoreEntity Book  {-^ The 'Edition' to link to. Must be a
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
