{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.EditionFormat (tests) where

import Test.BrainzStem

import Database.HDBC (toSql)

import BookBrainz.Model.EditionFormat
import BookBrainz.Types
import BrainzStem.Model

test_editionFormat_allEditionFormats :: DatabaseContext ()
test_editionFormat_allEditionFormats = do
  formats <- allEditionFormats
  liftIO $ do
    assertBool "Expected to find some formats" (length formats == 2)
    formats !! 0 @?= testFormat

test_editionFormat_getByPk :: DatabaseContext ()
test_editionFormat_getByPk = do
  editionFormat <- getByPk $ Ref (toSql (1 :: Int))
  liftIO $ editionFormat  @?= testFormat

testFormat = Entity { entityRef = Ref (toSql (1 :: Int))
                    , entityInfo = EditionFormat { editionFormatName = "Paperback" } }

tests :: [Test]
tests = [ testCase "allEditionFormats" $ databaseTest test_editionFormat_allEditionFormats
        , testCase "getByPk" $ databaseTest test_editionFormat_getByPk
        ]
