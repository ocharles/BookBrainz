{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Controller.Book
       ( bookResource
       ) where

import BookBrainz.Controller (output)
import BookBrainz.Model
import BookBrainz.Model.Book
import BookBrainz.Types.MVC (Controller)
import BookBrainz.Types.Newtypes
import BookBrainz.View.Book (showBook)
import Data.Maybe (fromJust)
import Snap.Types
import Data.ByteString.Char8 (unpack)

bookResource :: Controller ()
bookResource = do
  bid <- getParam "bid"
  book <- model $ getBook $ BookId $ read $ unpack $ fromJust $ bid
  output $ showBook $ fromJust book
