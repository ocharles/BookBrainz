{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Edition (tests) where

import Test.BrainzStem

import Data.Maybe (fromJust)
import Database.HDBC (toSql)

import BookBrainz.Model.Edition (findBookEditions)
import BookBrainz.Types
import BrainzStem.Model (getByBbid, create)

import BookBrainz.Model.Editor

testEdition = CoreEntity { bbid = fromJust $ parseBbid "86bb2a9d-ac90-4c92-9dc1-2398b4283df4"
                         , coreEntityRevision = Ref (toSql (45::Int))
                         , coreEntityTree = Ref (toSql (16::Int))
                         , coreEntityConcept = Ref (toSql (4::Int))
                         , coreEntityInfo = Edition { editionName = "Edition Edited"
                                                    , editionYear = Nothing
                                                    , editionPublisher = Just $ Ref (toSql (3 :: Int))
                                                    , editionCountry = Nothing
                                                    , editionLanguage = Just $ Ref (toSql ("eng" :: String))
                                                    , editionIsbn = Just $ read "9786000053147"
                                                    , editionFormat = Nothing
                                                    , editionBook = Ref (toSql (17 :: Int))
                                                    , editionIndex = Nothing
                                                    }
                         }

test_edition_getByBbid :: DatabaseContext ()
test_edition_getByBbid = do
  edition <- getByBbid $ fromJust $ parseBbid "86bb2a9d-ac90-4c92-9dc1-2398b4283df4"
  liftIO $ case edition of
    Just b -> b @?= testEdition
    Nothing -> assertFailure "Expected to find a edition"

test_edition_create :: DatabaseContext ()
test_edition_create = do
  createdEdition <- ((entityRef . fromJust) `fmap` getEditorByName "ocharles") >>=
    create (Edition { editionName = "My First Pony"
                    , editionYear = Nothing
                    , editionPublisher = Just $ Ref (toSql (3 :: Int))
                    , editionCountry = Nothing
                    , editionLanguage = Just $ Ref (toSql ("eng" :: String))
                    , editionIsbn = Just $ read "9786000053147"
                    , editionFormat = Nothing
                    , editionBook = Ref (toSql (17 :: Int))
                    , editionIndex = Nothing
                    })
  maybeFetchedEdition <- getByBbid $ bbid createdEdition
  liftIO $ case maybeFetchedEdition of
    Just fetchedEdition -> fetchedEdition @?= createdEdition
    Nothing -> assertFailure "Cannot retrieve created edition"

test_edition_findBookEditions :: DatabaseTest
test_edition_findBookEditions = do
  editions <- findBookEditions $ Ref (toSql (17 :: Int))
  liftIO $ do length editions @?= 2
              editions !!0 @?= testEdition

tests :: [Test]
tests = [ testCase "getByBbid" $ databaseTest test_edition_getByBbid
        , testCase "create" $ databaseTest test_edition_create
        , testCase "findBookEditions" $ databaseTest test_edition_findBookEditions
        ]

