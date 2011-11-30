{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Book (tests) where

import Test.BrainzStem

import Data.Maybe (fromJust)
import Database.HDBC (toSql)

import BookBrainz.Model.Book (listAllBooks, create)
import BookBrainz.Types
import BrainzStem.Model (getByBbid)

import BookBrainz.Model.Editor

test_book_listAllBooks :: DatabaseContext ()
test_book_listAllBooks = do
  books <- listAllBooks
  liftIO $ do
    assertBool "Expected to find some books" (length books == 3)
    (books !! 0) @?= testBook

testBook = CoreEntity { bbid = fromJust $ parseBbid "bcad4580-cfc7-47f6-a185-6fd1e0bb1079"
                      , coreEntityRevision = Ref (toSql (41::Int))
                      , coreEntityTree = Ref (toSql (24::Int))
                      , coreEntityConcept = Ref (toSql (17::Int))
                      , coreEntityInfo = Book { bookName = "A New Book" }
                      }

test_book_getByBbid :: DatabaseContext ()
test_book_getByBbid = do
  book <- getByBbid $ fromJust $ parseBbid "bcad4580-cfc7-47f6-a185-6fd1e0bb1079"
  liftIO $ case book of
    Just b -> b @?= testBook
    Nothing -> assertFailure "Expected to find a book"

test_book_create :: DatabaseContext ()
test_book_create = do
  createdBook <- ((entityRef . fromJust) `fmap` getEditorByName "ocharles") >>=
    create (Book { bookName = "My First Pony" })
  maybeFetchedBook <- getByBbid $ bbid createdBook
  liftIO $ case maybeFetchedBook of
    Just fetchedBook -> fetchedBook @?= createdBook
    Nothing -> assertFailure "Cannot retrieve created book"

tests :: [Test]
tests = [ testCase "listAllBooks" $ databaseTest test_book_listAllBooks
        , testCase "getByBbid" $ databaseTest test_book_getByBbid
        , testCase "create" $ databaseTest test_book_create
        ]

