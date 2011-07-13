{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Controller.Book
       ( bookResource
       , books
       ) where

import BookBrainz.Controller
import BookBrainz.Model
import BookBrainz.Model.Book
import BookBrainz.Model.Person
import BookBrainz.Lenses
import BookBrainz.Types.MVC (Controller)
import BookBrainz.Types
import BookBrainz.View.Book (showBook, showBooks)
import Control.Applicative
import Data.Traversable (traverse)
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromJust)
import Data.Record.Label
import Data.UUID (fromString)
import Snap.Types

bookResource :: Controller ()
bookResource = do
  gid  <- (fromString . unpack . fromJust <$> getParam "gid") `onNothing` "Invalid BBID"
  book <- (model $ getBook gid >>= traverse loadPeople) `onNothing` "Book not found"
  editions <- model $ findBookEditions book
  output $ showBook book editions

loadPeople :: Book -> Model Book
loadPeople book = do
  book' <- loadAuthorCredit book
  set lBookAuthorCredit book' <$> (loadForAuthorCredits $ bookAuthorCredit book')
  where set field on = \val -> setL field val on

books :: Controller ()
books = do
  bs <- model $ loadPeople `map'` listAllBooks
  output $ showBooks bs
  where map' f xs = mapM f =<< xs
