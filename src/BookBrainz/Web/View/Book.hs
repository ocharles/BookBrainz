{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.Web.View.Book
       ( addBook
       , showBook
       , showBooks
       ) where

import Data.Copointed
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, toValue, Html, (!))
import Text.Blaze.Html5.Attributes as A
import Data.UUID (toString)
import Text.Digestive.Forms.Html (FormEncType)

import BookBrainz.Types
import BookBrainz.Web.View (pageLayout)
import BookBrainz.Web.View.Edition (linkEdition)

showBook :: LoadedCoreEntity Book -> [LoadedCoreEntity Edition] -> Html
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

showBooks :: [LoadedCoreEntity Book] -> Html
showBooks books =
  pageLayout $ do
    H.h1 "Books"
    H.ul $ (H.li . bookLink) `mapM_` books
    where bookLink book =
            let uri = "/book/" ++ toString (gid book) in
            H.a ! href (toValue uri) $ toHtml $ bookName $ copoint book

addBook :: (Html, FormEncType) -> Html
addBook (formHtml, enctype) =
  pageLayout $ do
    H.h1 "Add Book"
    H.form ! method "POST" $ do
      formHtml
      H.p $ H.input ! A.type_ "submit" ! A.value "Add Book"
