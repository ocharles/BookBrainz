{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Publisher (tests) where

import Test.BrainzStem

import Data.Maybe (fromJust)
import Database.HDBC (toSql)

import BookBrainz.Model.Publisher
import BookBrainz.Types
import BrainzStem.Model (getByBbid, create)

import BookBrainz.Model.Editor

test_publisher_allPublishers :: DatabaseContext ()
test_publisher_allPublishers = do
  publishers <- allPublishers
  liftIO $ do
    assertBool "Expected to find some people" (length publishers == 1)
    (publishers !! 0) @?= testPublisher

testPublisher = CoreEntity { bbid = fromJust $ parseBbid "a4053a91-312a-4c5f-8eef-030a0caa18ad"
                           , coreEntityRevision = Ref (toSql (42::Int))
                           , coreEntityTree = Ref (toSql (3::Int))
                           , coreEntityConcept = Ref (toSql (3::Int))
                           , coreEntityInfo = Publisher { publisherName = "Bob Publishing Inc" }
                           }

test_publisher_getByBbid :: DatabaseContext ()
test_publisher_getByBbid = do
  publisher <- getByBbid $ fromJust $ parseBbid "a4053a91-312a-4c5f-8eef-030a0caa18ad"
  liftIO $ case publisher of
    Just b -> b @?= testPublisher
    Nothing -> assertFailure "Expected to find a publisher"

test_publisher_create :: DatabaseContext ()
test_publisher_create = do
  createdPublisher <- ((entityRef . fromJust) `fmap` getEditorByName "ocharles") >>=
    create (Publisher { publisherName = "My First Pony" })
  maybeFetchedPublisher <- getByBbid $ bbid createdPublisher
  liftIO $ case maybeFetchedPublisher of
    Just fetchedPublisher -> fetchedPublisher @?= createdPublisher
    Nothing -> assertFailure "Cannot retrieve created publisher"

test_publisher_publishedEditions :: DatabaseTest
test_publisher_publishedEditions = do
  publisher <- fromJust `fmap` (getByBbid $ fromJust $ parseBbid "a4053a91-312a-4c5f-8eef-030a0caa18ad")
  editions <- publishedEditions $ coreEntityConcept publisher
  liftIO $ do
    length editions @?= 1
    bbid (editions !! 0) @?= (fromJust $ parseBbid "86bb2a9d-ac90-4c92-9dc1-2398b4283df4")

tests :: [Test]
tests = [ testCase "allPublishers" $ databaseTest test_publisher_allPublishers
        , testCase "getByBbid" $ databaseTest test_publisher_getByBbid
        , testCase "create" $ databaseTest test_publisher_create
        , testCase "publishedEditions" $ databaseTest test_publisher_publishedEditions
        ]

