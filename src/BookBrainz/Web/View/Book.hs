{-# LANGUAGE OverloadedStrings #-}

-- | Views for 'Book's.
module BookBrainz.Web.View.Book
       ( -- * Pages
         addBook
       , showBook
       , showBooks
       ) where

import           Data.Copointed
import           Text.Blaze.Html5          (toHtml, toValue, Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Forms.Html (FormEncType)

import           BookBrainz.Types
import           BookBrainz.Web.View       (pageLayout, linkEdition, linkBook
                                           ,linkPublisher, detailTable)
import qualified BookBrainz.Web.View.Sidebar as Sidebar

--------------------------------------------------------------------------------
-- | Display a single 'Book'.
showBook :: (LoadedCoreEntity Book
            ,[(LoadedEntity Role, LoadedCoreEntity Person)]
            )
         -- ^ The 'Book' to display.
         -> [( LoadedCoreEntity Edition
             , Maybe (LoadedCoreEntity Publisher)
             )]
         -- ^ A list of 'Edition's for this 'Book'.
         -> Html
showBook (book, roles) editions =
  pageLayout (Just sidebar) $ do
    let book' = copoint book
    H.h1 $ toHtml $ bookName book'
    H.h3 "Editions"
    detailTable
      [("Name", [])
      ,("Year", [])
      ,("ISBN", [])
      ,("Publisher",[])]
      (editionRow `map` editions)
  where
    maybeCell f = toHtml . maybe "-" f
    editionRow (edition, publisher) =
      let edition' = copoint edition in
      [ toHtml $ linkEdition edition
      , maybeCell toHtml $ editionYear edition'
      , maybeCell toHtml $ editionIsbn edition'
      , maybeCell linkPublisher publisher
      ]
    sidebar = Sidebar.roles roles

--------------------------------------------------------------------------------
-- | Display a list of many 'Book's.
showBooks :: [LoadedCoreEntity Book]  -- ^ The 'Book's to display.
          -> Html
showBooks books =
  pageLayout Nothing $ do
    H.h1 "Books"
    H.ul $ (H.li . linkBook) `mapM_` books

--------------------------------------------------------------------------------
-- | A form for adding new 'Book's.
addBook :: (Html, FormEncType)  -- ^ The form 'Html' and the encoding of it.
        -> Html
addBook (formHtml, enctype) =
  pageLayout Nothing $ do
    H.h1 "Add Book"
    H.form ! A.method "POST" ! A.enctype (toValue enctype) $ do
      formHtml
      H.p $ H.input ! A.type_ "submit" ! A.value "Add Book"
