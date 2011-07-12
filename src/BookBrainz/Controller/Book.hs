{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Controller.Book
       ( bookResource
       ) where

import BookBrainz.Controller
import BookBrainz.Model
import BookBrainz.Model.Book
import BookBrainz.Model.Person
import BookBrainz.Lenses
import BookBrainz.Types.MVC (Controller)
import BookBrainz.Types
import BookBrainz.View.Book (showBook)
import Control.Applicative
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromJust)
import Data.Record.Label
import Data.UUID (fromString)
import Snap.Types


bookResource :: Controller ()
bookResource = do
  gid  <- (fromString . unpack . fromJust <$> getParam "gid") `onNothing` "Invalid BBID"
  book <- (model $ getBook gid) `onNothing` "Book not found"
  book <- model $ loadAuthorCredit book
  book <- model $ set lBookAuthorCredit book <$> (loadForAuthorCredits $ bookAuthorCredit book)
  editions <- model $ findBookEditions book
  output $ showBook book editions
  where set field on = \val -> setL field val on
