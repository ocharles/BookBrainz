{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for the @/book@ resource
module BookBrainz.Web.Handler.Book
       ( listBooks
       , showBook
       , addBook
       ) where

import           Data.ByteString.Char8 (pack)
import           Data.UUID
import           Snap.Types
import           Text.Digestive.Forms.Snap
import           Text.Digestive.Blaze.Html5

import qualified BookBrainz.Forms as Forms
import           BookBrainz.Model
import           BookBrainz.Model.Book
import           BookBrainz.Model.Edition
import           BookBrainz.Web.Handler (output, onNothing)
import           BookBrainz.Web.Snaplet (BookBrainzHandler, database)
import           BookBrainz.Web.Snaplet.Database (withTransaction)
import qualified BookBrainz.Web.View.Book as V
import           BrainzStem.Types (gid)

--------------------------------------------------------------------------------
-- | List all known books.
listBooks :: BookBrainzHandler ()
listBooks = do
  bs <- listAllBooks
  output $ V.showBooks bs

--------------------------------------------------------------------------------
{-| Show a single 'Book', searching by it's GID. If the book cannot be found,
a 404 page is displayed. -}
showBook :: UUID -> BookBrainzHandler ()
showBook bbid = do
  book <- getByGid bbid `onNothing` "Book not found"
  editions <- findBookEditions book
  output $ V.showBook book editions

--------------------------------------------------------------------------------
{-| Display a form for adding 'Book's, and on submission add that book and
redirect to view it. -}
addBook :: BookBrainzHandler ()
addBook = do
  r <- eitherSnapForm Forms.bookForm "book"
  case r of
    Left form' -> output $ V.addBook $ renderFormHtml form'
    Right submission -> do
      book <- withTransaction database $ insertBook submission
      redirect $ pack . ("/book/" ++) . toString . gid $ book
