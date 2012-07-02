{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Book (tests) where

import Test.BrainzStem

import Data.Maybe (fromJust)

import BookBrainz.Model.Book (listAllBooks)
import BookBrainz.Types

test_book_listAllBooks :: DatabaseContext ()
test_book_listAllBooks = do
  books <- listAllBooks
  liftIO $ do
    assertBool "Expected to find some books" (length books == 1)
    (books !! 0) @?= testBook

testBook :: LoadedCoreEntity Book
testBook = CoreEntity { bbid = fromJust $ parseBbid "08045dd2-84d1-4aba-85f0-ca155e16c3cc"
                      , coreEntityRevision = BookRevisionRef 2
                      , coreEntityTree = BookTreeRef 1
                      , coreEntityInfo = Book { bookName = "A New Book" }
                      }

-- test_book_getByBbid :: DatabaseContext ()
-- test_book_getByBbid = do
--   book <- getByBbid $ fromJust $ parseBbid "bcad4580-cfc7-47f6-a185-6fd1e0bb1079"
--   liftIO $ case book of
--     Just b -> b @?= testBook
--     Nothing -> assertFailure "Expected to find a book"

-- test_book_create :: DatabaseContext ()
-- test_book_create = do
--   createdBook <- ((entityRef . fromJust) `fmap` getEditorByName "ocharles") >>=
--     create (Book { bookName = "My First Pony" })
--   maybeFetchedBook <- getByBbid $ bbid createdBook
--   liftIO $ case maybeFetchedBook of
--     Just fetchedBook -> fetchedBook @?= createdBook
--     Nothing -> assertFailure "Cannot retrieve created book"

-- test_book_update :: DatabaseTest
-- test_book_update = do
--   original <- fetchBook
--   editor <- ((entityRef . fromJust) `fmap` getEditorByName "ocharles")
--   master <- findMasterBranch $ coreEntityConcept original
--   let newBook = (coreEntityInfo original) { bookName = "Renamed book" }
--   changeBranch master editor $ update newBook

--   commitedBook <- fetchBook
--   tipRoles <- findRoles (coreEntityTree commitedBook)
--   prevRoles <- findRoles (coreEntityTree original)
--   liftIO $ do (coreEntityInfo commitedBook) @?= newBook
--               tipRoles @?= prevRoles
--   where fetchBook = fromJust `fmap`
--           (getByBbid $ fromJust $ parseBbid "bcad4580-cfc7-47f6-a185-6fd1e0bb1079")

tests :: [Test]
tests = [ testCase "listAllBooks" $ databaseTest test_book_listAllBooks
        -- , testCase "getByBbid" $ databaseTest test_book_getByBbid
        -- , testCase "create" $ databaseTest test_book_create
        -- , testCase "update" $ databaseTest test_book_update
        ]

