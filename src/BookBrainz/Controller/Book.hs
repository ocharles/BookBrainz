{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Controller.Book
       ( bookResource
       ) where

import BookBrainz.Controller (output, generic404)
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
  mbook <- model $ getBook $ BookId $ read $ unpack $ fromJust $ bid
  maybe (generic404 "The request book could not be found")
        (\book -> do
            book <- model $ loadAuthorCredit book
            editions <- model $ findBookEditions book
            output $ showBook book editions)
        mbook
