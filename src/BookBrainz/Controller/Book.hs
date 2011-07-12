{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Controller.Book
       ( bookResource
       ) where

import BookBrainz.Controller (output, generic404, genericError)
import BookBrainz.Model
import BookBrainz.Model.Book
import BookBrainz.Model.Person
import BookBrainz.Types.MVC (Controller)
import BookBrainz.Types
import BookBrainz.View.Book (showBook)
import Control.Applicative
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromJust)
import Data.UUID (fromString)
import Snap.Types


bookResource :: Controller ()
bookResource = do
  maybeGid <- (fromString . unpack . fromJust) <$> getParam "gid"
  case maybeGid of
    Nothing -> genericError 400 "Invalid BBID"
    Just gid -> do
      maybeBook <- model $ getBook gid
      case maybeBook of
        Nothing -> generic404 "The request book could not be found"
        Just book -> do
          book <- model $ loadAuthorCredit book
          book <- model $ (\credits -> book { bookAuthorCredit = credits }) <$> (loadForAuthorCredits $ bookAuthorCredit book)
          editions <- model $ findBookEditions book
          output $ showBook book editions
