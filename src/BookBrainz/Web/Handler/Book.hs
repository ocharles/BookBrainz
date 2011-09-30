{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Handlers for the @/book@ resource.
module BookBrainz.Web.Handler.Book
       ( listBooks
       , showBook
       , addBook
       , editBook
       ) where

import           Control.Applicative        ((<$>))
import           Data.Traversable           (traverse)

import           Data.Copointed             (copoint)

import           BookBrainz.Model.Book
import           BookBrainz.Model.Edition
import           BookBrainz.Model.Publisher ()
import           BookBrainz.Model.Role      (findRoles)
import           BookBrainz.Types           (coreEntityTree, editionPublisher, BBID)
import           BookBrainz.Web.Handler     (output, onNothing)
import           BookBrainz.Web.Snaplet     (BookBrainzHandler)
import qualified BookBrainz.Web.View.Book   as V
import           BrainzStem.Model

--------------------------------------------------------------------------------
-- | List all known books.
listBooks :: BookBrainzHandler ()
listBooks = do
  bs <- listAllBooks
  output $ V.showBooks bs

--------------------------------------------------------------------------------
{-| Show a single 'Book', searching by its BBID. If the book cannot be found,
a 404 page is displayed. -}
showBook :: BBID -> BookBrainzHandler ()
showBook bbid = do
  book <- getByBbid bbid `onNothing` "Book not found"
  editions <- findBookEditions (coreEntityTree book) >>= mapM loadEdition
  roles <- findRoles (coreEntityTree book)
  output $ V.showBook (book, roles) editions
  where loadEdition e =
          (e, ) <$> traverse getByConcept (editionPublisher . copoint $ e)

addBook :: BookBrainzHandler ()
addBook = undefined

editBook :: BBID -> BookBrainzHandler ()
editBook = undefined
