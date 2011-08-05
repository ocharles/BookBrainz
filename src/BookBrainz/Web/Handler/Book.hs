{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.Handler.Book
       ( listBooks
       , showBook
       ) where

import           Data.UUID

import           BookBrainz.Model.Book
import           BookBrainz.Web.Handler (output, onNothing)
import           BookBrainz.Web.Snaplet (BookBrainzHandler)
import qualified BookBrainz.Web.View.Book as V

listBooks :: BookBrainzHandler ()
listBooks = do
  bs <- listAllBooks
  output $ V.showBooks bs

showBook :: UUID -> BookBrainzHandler ()
showBook bbid = do
  book <- getBook bbid `onNothing` "Book not found"
  editions <- findBookEditions book
  output $ V.showBook book editions
