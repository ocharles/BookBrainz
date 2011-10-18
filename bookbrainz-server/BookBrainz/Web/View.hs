{-# LANGUAGE OverloadedStrings #-}

-- | Generic views, common to many pages.
module BookBrainz.Web.View
       ( genericError
       , pageLayout
       , runView
       , View

         -- * Access to the View Environment
       , currentEditor

         -- * Linking
       , linkBook
       , linkEdition
       , linkPerson
       , linkPublisher

         -- * Components
       , optionalDl
       , detailTable
       ) where

import           Control.Monad                     (when)
import           Data.Functor.Identity             (Identity)
import           Data.Maybe                        (isJust)
import           Data.Monoid                       (mempty)

import           Control.Monad.Reader              (Reader, runReader, asks, ReaderT)
import           Data.Copointed
import           Data.Text                         (Text)
import           Text.Blaze.Html5                  (Html, toHtml, (!), toValue
                                                   ,ToValue
                                                   ,preEscapedStringValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Forms.Html         (FormEncType)

import           BookBrainz.Types
import           BookBrainz.Web.Sitemap as Sitemap (Sitemap(..), showURL)

--------------------------------------------------------------------------------
-- | Display a generic error page.
genericError :: Text -> View
genericError message =
  pageLayout Nothing $ do
    H.h1 "Oops!"
    H.p $ toHtml message

--------------------------------------------------------------------------------
data ViewData = ViewData { vdCurrentEditor :: Maybe (LoadedEntity Editor) }

-- | A page on the web site, with access to the current request environment.
type View = Reader ViewData Html

-- | Run a 'View' against a specific environment, generating 'Html'.
runView :: View -> Maybe (LoadedEntity Editor) -> Html
runView view = runReader view . ViewData

-- | Get the current 'Editor' for rendering this 'View'. May be 'Nothing' if no
-- editor is logged in.
currentEditor :: ReaderT ViewData Identity (Maybe (LoadedEntity Editor))
currentEditor = asks vdCurrentEditor

--------------------------------------------------------------------------------
{-| The common layout for most BookBrainz pages, complete with heading, footer,
navigation links, etc. -}
pageLayout :: Maybe Html   -- ^ Optional 'Html' for the sidebar.
           -> Html         -- ^ The main content of the page.
           -> View
pageLayout sidebar body = do
  cu <- currentEditor
  let loggedIn = isJust cu
  return . H.docTypeHtml $ do
    H.head $ do
      H.title "BookBrainz"
      H.link ! A.type_ "text/css" ! A.rel "stylesheet"
             ! (A.href . toValue . showURL $ Resource "style.css")
    H.body $ do
      H.div ! A.id "header" $ do
        H.div ! A.id "header-logo" $ do
          navLink Home H.img
          H.div ! A.id "header-search" $
            H.form ! A.method "GET" ! A.action (toValue $ showURL Search) $ do
              H.input ! A.name "search-fval[0]" ! A.placeholder "search"
              H.input ! A.type_ "submit"
        H.div ! A.id "header-menu" $
          H.div $ do
            H.ul ! A.class_ "nav-left" $ do
              H.li $ navLink Home "BookBrainz" -- TODO Navigation menu
              when loggedIn $ do
                H.li $ navLink AddBook "Add Book"
            H.ul ! A.class_ "nav-right" $
              case cu of
                Just _ -> do
                  H.li $ navLink Logout "Logout"
                Nothing -> do
                  H.li $ navLink Login "Login"
                  H.li $ navLink Register "Register"
      H.div ! A.id "page" $ do
        showSidebar sidebar
        H.div ! A.id "content" $ body -- TODO Sidebar
      H.div ! A.id "footer" $ mempty -- TODO Footer links
  where navLink page = H.a ! A.href (toValue $ showURL page)
        showSidebar (Just s) = H.div ! A.id "sidebar" $ s
        showSidebar _ = mempty

--------------------------------------------------------------------------------
-- | Link to a book.
linkBook :: LoadedCoreEntity Book  {-^ The 'Book' to link to. Must be a
                                   'LoadedCoreEntity' in order to have a
                                   BBID. -}
         -> Html
linkBook book =
  let uri = showURL $ Sitemap.Book (bbid book) in
  H.a ! A.href (toValue uri) $ toHtml $ bookName $ copoint book

--------------------------------------------------------------------------------
-- | Link to an edition.
linkEdition :: LoadedCoreEntity Edition  {-^ The 'Edition' to link to. Must be a
                                         'LoadedCoreEntity' in order to have a
                                         BBID. -}
            -> Html
linkEdition edition =
  let uri = showURL $ Sitemap.Edition (bbid edition) in
  H.a ! A.href (toValue uri) $ toHtml $ (editionName . copoint) edition

--------------------------------------------------------------------------------
-- | Link to a publisher.
linkPublisher :: LoadedCoreEntity Publisher
              -- ^ The 'Edition' to link to. Must be a 'LoadedCoreEntity' in
              -- order to have a BBID.
              -> Html
linkPublisher p =
  let uri = showURL $ Sitemap.Publisher (bbid p) in
  H.a ! A.href (toValue uri) $ toHtml $ publisherName $ copoint p

--------------------------------------------------------------------------------
-- | Link to a person.
linkPerson :: LoadedCoreEntity Person
           -- ^ The 'Person' to link to. Must be a 'LoadedCoreEntity' in
           -- order to have a BBID.
           -> Html
linkPerson p =
  let uri = showURL $ Sitemap.Person (bbid p) in
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

--------------------------------------------------------------------------------
instance ToValue FormEncType where
  toValue = preEscapedStringValue . show
