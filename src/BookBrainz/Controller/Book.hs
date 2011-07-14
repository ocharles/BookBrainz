{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Controller.Book
       ( bookResource
       , books
       ) where

import BookBrainz.Controller
import BookBrainz.Model
import BookBrainz.Model.AuthorCredit
import BookBrainz.Model.Book
import BookBrainz.Types.MVC (Controller)
import BookBrainz.Types hiding (gid)
import BookBrainz.View.Book (showBook, showBooks)
import Control.Applicative
import Data.ByteString.Char8 (unpack)
import Data.Copointed
import Data.Maybe (fromJust)
import Data.UUID (fromString)
import Snap.Types

bookResource :: Controller ()
bookResource = do
  gid  <- (fromString . unpack . fromJust <$> getParam "gid") `onNothing` "Invalid BBID"
  book <- model (getBook gid) `onNothing` "Book not found"
  author <- model $ getAuthorCredit $ bookAuthorCredit $ copoint book
  editions <- model $ findBookEditions $ copoint book
  output $ showBook (book, author) editions

books :: Controller ()
books = do
  bs <- model listAllBooks
  output $ showBooks bs
