{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Handlers for the @/book@ resource.
module BookBrainz.Web.Handler.Book
       ( listBooks
       , showBook
       , addBook
       , editBook
       , addBookRole
       ) where

import           Control.Applicative        ((<$>))
import           Data.Traversable           (traverse)

import           Data.ByteString.Char8      (pack)
import           Data.Copointed             (copoint)
import           Snap.Core                  (redirect)
import           Snap.Snaplet.PostgresqlSimple (withTransaction)
import           Text.Digestive.Snap (runForm)

import qualified BookBrainz.Forms as Forms
import           BookBrainz.Model.Book      (create, listAllBooks)
import           BookBrainz.Model.Edition
import           BookBrainz.Model.Publisher ()
import           BookBrainz.Model.Role      (findRoles, addRole)
import           BookBrainz.Types
import           BookBrainz.Web.Handler     (output, onNothing, withUser)
import           BookBrainz.Web.Snaplet     (BookBrainzHandler)
import qualified BookBrainz.Web.View.Book   as V
import qualified BookBrainz.Web.View.Role   as V
import           BrainzStem.Model           (getByBbid, getByConcept, update
                                            ,findMasterBranch, changeBranch)

--------------------------------------------------------------------------------
-- | List all known books.
listBooks :: BookBrainzHandler ()
listBooks = do
  bs <- listAllBooks
  output $ V.showBooks bs

--------------------------------------------------------------------------------
{-| Show a single 'Book', searching by its BBID. If the book cannot be found,
a 404 page is displayed. -}
showBook :: BBID Book -> BookBrainzHandler ()
showBook bbid' = do
  book <- getByBbid bbid' `onNothing` "Book not found"
  editions <- findBookEditions (coreEntityConcept book) >>= mapM loadEdition
  roles <- findRoles (coreEntityTree book)
  output $ V.showBook (book, roles) editions
  where loadEdition e =
          (e, ) <$> traverse getByConcept (editionPublisher . copoint $ e)

---------------------------------------------------------------------------------
{-| Display a form for adding 'Book's, and on submission, add that book and
redirect to view it. -}
addBook :: BookBrainzHandler ()
addBook =
  withUser $ \user -> do
    (v, r) <- runForm "book" (Forms.book Nothing)
    case r of
      Nothing -> output $ V.addBook v
      Just submission -> do
        book <- withTransaction $ create submission $ entityRef user
        redirect $ pack . ("/book/" ++) . show . bbid $ book

---------------------------------------------------------------------------------
{-| Display a form for adding 'Book's, and on submission, add that book and
redirect to view it. -}
editBook :: BBID Book -> BookBrainzHandler ()
editBook bbid' =
  withUser $ \user -> do
    book <- getByBbid bbid' `onNothing` "Book not found"
    (v, r) <- runForm "book" (Forms.book . Just $ copoint book)
    case r of
      Nothing -> output $ V.addBook v
      Just submission -> do
        withTransaction $ do
          master <- findMasterBranch $ coreEntityConcept book
          changeBranch master (entityRef user) $
            update submission
        redirect $ pack . ("/book/" ++) . show . bbid $ book

--------------------------------------------------------------------------------
{-| Present an interface for adding a new role to this book. -}
addBookRole :: BBID Book -> BookBrainzHandler ()
addBookRole bbid' =
  withUser $ \user -> do
    book <- getByBbid bbid' `onNothing` "Book not found"
    roleForm <- Forms.personRole
    (v, r) <- runForm "book" roleForm
    case r of
      Nothing -> output $ V.addRole v
      Just submission -> do
        withTransaction $ do
          master <- findMasterBranch $ coreEntityConcept book
          changeBranch master (entityRef user) $
            addRole submission
        redirect $ pack . ("/book/" ++) . show . bbid $ book
