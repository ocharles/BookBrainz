{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.View.Book
       ( pageLayout
       , showBook
       , showBooks
       ) where

import BookBrainz.Types
import BookBrainz.View (pageLayout)
import BookBrainz.View.AuthorCredit (linkAuthorCredit)
import BookBrainz.View.Edition (linkEdition)
import Data.Copointed
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, toValue, Html, (!))
import Text.Blaze.Html5.Attributes
import Data.UUID (toString)

showBook :: (LoadedCoreEntity Book, LoadedEntity AuthorCredit) -> [LoadedCoreEntity Edition] -> Html
showBook (book, authorCredit) editions =
  pageLayout $ do
    let book' = copoint book
    H.h1 $ toHtml $ bookName book'
    H.h2 $ do
      H.span "~"
      " "
      linkAuthorCredit authorCredit
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
