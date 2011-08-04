{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Controller.Book where

import BookBrainz.Controller
import BookBrainz.Model
import BookBrainz.Model.AuthorCredit
import BookBrainz.Model.Book
import BookBrainz.Types.MVC (Controller)
import BookBrainz.Types
import qualified BookBrainz.View.Book as V
import Control.Applicative
import Data.ByteString.Char8 (pack)
import Data.Text.Encoding as E
import Data.UUID (toString, UUID)
import Snap.Types

bookResource :: UUID -> Controller ()
bookResource bbid = do
  book <- model (getBook bbid) `onNothing` "Book not found"
  editions <- model $ findBookEditions book
  output $ V.showBook book editions

books :: Controller ()
books = do
  bs <- model listAllBooks
  output $ V.showBooks bs

addBookForm :: Controller ()
addBookForm = output V.addBook

addBook :: Controller ()
addBook = do
  method <- rqMethod <$> getRequest
  case method of
    POST -> handlePost
    GET  -> addBookForm
  where handlePost = do
          name   <- E.decodeUtf8 <$> getParam "book.name" `onNothing` "No book name"
          author <- E.decodeUtf8 <$> getParam "book.author" `onNothing` "No book author"
          credit <- model $ insertSimpleAuthorCredit author
          book   <- model $ insertBook Book { bookName = name }
          redirect $ pack $ toString $ gid book
