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
import           BookBrainz.Types                (gid, editionPublisher
                                                 ,coreEntityTree)
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
  editions <- findBookEditions (coreEntityTree book) >>= mapM loadEdition
  roles <- findRoles (coreEntityTree book)
  output $ V.showBook (book, roles) editions
  where loadEdition e =
          (e, ) <$> traverse getByConcept (editionPublisher . copoint $ e)

addBook = undefined
editBook = undefined
