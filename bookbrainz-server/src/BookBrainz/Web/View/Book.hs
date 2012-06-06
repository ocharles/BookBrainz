{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Views for 'Book's.
module BookBrainz.Web.View.Book
       ( -- * Pages
         addBook
       , showBook
       , showBooks
       , addRole
       ) where

import           Data.Copointed
import           Text.Blaze.Html5          (toHtml, toValue, Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Blaze.Html5
import qualified Text.Digestive.View as Form

import           BookBrainz.Types
import           BookBrainz.Web.View       (pageLayout, linkBook, View)
import           BookBrainz.Web.View.Edition (editionTable)
import qualified BookBrainz.Web.View.Sidebar as Sidebar
import           BookBrainz.Web.Sitemap    (Sitemap(..), showURL)

--------------------------------------------------------------------------------
-- | Display a single 'Book'.
showBook :: (LoadedCoreEntity Book
            ,[ LoadedEntity Role :. LoadedCoreEntity Person ]
            )
         -- ^ The 'Book' to display.
         -> [( LoadedCoreEntity Edition
             , Maybe (LoadedCoreEntity Publisher)
             )]
         -- ^ A list of 'Edition's for this 'Book'.
         -> View
showBook (book, roles) editions =
  pageLayout (Just sidebar) $ do
    let book' = copoint book
    H.h1 $ toHtml $ bookName book'
    H.h3 "Editions"
    editionTable editions
    H.p $
      H.ul $ do
        H.li $ H.a ! A.href (toValue . showURL $ AddEdition $ bbid book) $
                 "Add a new edition"
        H.li $ H.a ! A.href (toValue . showURL $ EditBook $ bbid book) $
                 "Edit this book"
        H.li $ H.a ! A.href (toValue . showURL $ AddBookRole $ bbid book) $
                 "Add a new person-role"
  where sidebar = Sidebar.roles roles

--------------------------------------------------------------------------------
-- | Display a list of many 'Book's.
showBooks :: [LoadedCoreEntity Book]  -- ^ The 'Book's to display.
          -> View
showBooks books =
  pageLayout Nothing $ do
    H.h1 "Books"
    H.ul $ (H.li . linkBook) `mapM_` books

--------------------------------------------------------------------------------
-- | A form for adding new 'Book's.
addBook :: Form.View Html  -- ^ The form 'Html' and the encoding of it.
        -> View
addBook v =
  pageLayout Nothing $ do
    H.h1 "Add Book"
    H.form ! A.method "POST" ! A.enctype (H.toValue $ Form.viewEncType v) $ do
      H.p $ do
        label "title" v "Title:"
        inputText "title" v
      H.p $ H.input ! A.type_ "submit" ! A.value "Add Book"

--------------------------------------------------------------------------------
-- | A form for adding new 'Role's to a 'Book'.
addRole :: Form.View Html  -- ^ The form 'Html' and the encoding of it.
        -> View
addRole v =
  pageLayout Nothing $ do
    H.h1 "Add Role"
    H.form ! A.method "POST" ! A.enctype (H.toValue $ Form.viewEncType v) $ do
      H.p $ do
        label "person" v "Person:"
        inputSelect "person" v
      H.p $ do
        label "role" v "Role:"
        inputSelect "role" v
      H.p $ H.input ! A.type_ "submit" ! A.value "Add Role"
