{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Handlers for the @/book@ resource.
module BookBrainz.Web.Handler.Book
       ( listBooks
       , showBook
       , addBook
       , editBook
       ) where

import           Control.Applicative             ((<$>))
import           Data.ByteString.Char8           (pack)
import           Data.Traversable                (traverse)

import           Data.Copointed                  (copoint)
import           Data.UUID
import           Snap.Types
import           Text.Digestive.Forms.Snap
import           Text.Digestive.Blaze.Html5

import           BrainzStem.Model
import qualified BookBrainz.Forms as Forms
import           BookBrainz.Model.Book
import           BookBrainz.Model.Edition
import           BookBrainz.Model.Publisher      ()
import           BookBrainz.Model.Role           (findRoles)
import           BookBrainz.Types                (gid, editionPublisher)
import           BookBrainz.Web.Handler          (output, onNothing)
import           BookBrainz.Web.Snaplet          (BookBrainzHandler, database)
import           BookBrainz.Web.Snaplet.Database (withTransaction)
import qualified BookBrainz.Web.View.Book as V

--------------------------------------------------------------------------------
-- | List all known books.
listBooks :: BookBrainzHandler ()
listBooks = do
  bs <- listAllBooks
  output $ V.showBooks bs

--------------------------------------------------------------------------------
{-| Show a single 'Book', searching by its GID. If the book cannot be found,
a 404 page is displayed. -}
showBook :: UUID -> BookBrainzHandler ()
showBook bbid = do
  book <- getByGid bbid `onNothing` "Book not found"
  editions <- findBookEditions book >>= mapM loadEdition
  roles <- findRoles book
  output $ V.showBook (book, roles) editions
  where loadEdition e =
          (e, ) <$> traverse getById (editionPublisher . copoint $ e)

--------------------------------------------------------------------------------
{-| Display a form for adding 'Book's, and on submission, add that book and
redirect to view it. -}
addBook :: BookBrainzHandler ()
addBook = do
  r <- eitherSnapForm (Forms.bookForm Nothing) "book"
  case r of
    Left form' -> output $ V.addBook $ renderFormHtml form'
    Right submission -> do
      book <- withTransaction database $ insert submission
      redirect $ pack . ("/book/" ++) . toString . gid $ book

--------------------------------------------------------------------------------
{-| Display a form for editing a 'Book', and on submission, edit that book and
redirect to view it. -}
editBook :: UUID -> BookBrainzHandler ()
editBook bbid = do
  book <- getByGid bbid `onNothing` "Book not found"
  r <- eitherSnapForm (Forms.bookForm $ Just $ copoint book) "book"
  case r of
    Left form' -> output $ V.addBook $ renderFormHtml form'
    Right submission -> do
      withTransaction database $ do
        master <- findMasterBranch book
        update book submission (Just master)
      redirect $ pack . ("/book/" ++) . toString . gid $ book
