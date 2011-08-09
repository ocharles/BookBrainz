{-# LANGUAGE OverloadedStrings #-}

-- | View's for 'Book's.
module BookBrainz.Web.View.Book
       ( -- * Pages
         addBook
       , showBook
       , showBooks
       ) where

import           Data.Copointed
import           Data.UUID                   (toString)
import           Text.Blaze.Html5            (toHtml, toValue, Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Forms.Html   (FormEncType)

import BookBrainz.Types
import BookBrainz.Web.View         (pageLayout)
import BookBrainz.Web.View.Edition (linkEdition)

--------------------------------------------------------------------------------
-- | Display a single 'Book'.
showBook :: LoadedCoreEntity Book      -- ^ The 'Book' to display.
         -> [LoadedCoreEntity Edition] -- ^ A list of this 'Book's 'Edition's.
         -> Html
showBook book editions =
  pageLayout $ do
    let book' = copoint book
    H.h1 $ toHtml $ bookName book'
    H.h3 "Editions"
    H.table $ do
      H.thead $
        H.tr $ do
          H.th "Name"
          H.th "Year"
      H.tbody $ editionRow `mapM_` editions
      where editionRow edition =
              let edition' = copoint edition in
              H.tr $ do
                H.td $ toHtml $ linkEdition edition
                H.td $ toHtml $ maybe "-" show $ editionYear edition'

--------------------------------------------------------------------------------
-- | Display a list of many 'Book's.
showBooks :: [LoadedCoreEntity Book]  -- ^ The 'Book's to display.
          -> Html
showBooks books =
  pageLayout $ do
    H.h1 "Books"
    H.ul $ (H.li . bookLink) `mapM_` books
    where bookLink book =
            let uri = "/book/" ++ toString (gid book) in
            H.a ! A.href (toValue uri) $ toHtml $ bookName $ copoint book

--------------------------------------------------------------------------------
-- | A form for adding new 'Book's.
addBook :: (Html, FormEncType)  -- ^ The form 'Html', and the encoding of it
        -> Html
addBook (formHtml, enctype) =
  pageLayout $ do
    H.h1 "Add Book"
    H.form ! A.method "POST" $ do
      formHtml
      H.p $ H.input ! A.type_ "submit" ! A.value "Add Book"
