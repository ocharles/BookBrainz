{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.EditionFormat (tests) where

import Test.BrainzStem

import Database.HDBC (toSql)
import Data.Text (Text)

import BookBrainz.Model.EditionFormat
import BookBrainz.Types
import BrainzStem.Model

test_editionFormat_allEditionFormats :: DatabaseContext ()
test_editionFormat_allEditionFormats = do
  runRaw initialEnvironment
  formats <- allEditionFormats
  liftIO $ do
    assertBool "Expected to find some formats" (length formats == 2)
    formats !! 0 @?= testFormat 1 "Paperback"
    formats !! 1 @?= testFormat 2 "Hard back"

test_editionFormat_getByPk :: DatabaseContext ()
test_editionFormat_getByPk = do
  runRaw initialEnvironment
  loadedEditionFormat <- getByPk $ Ref (toSql (1 :: Int))
  liftIO $ loadedEditionFormat @?= testFormat 1 "Paperback"

initialEnvironment :: String
initialEnvironment = [str|
  INSERT INTO edition_format (id, name) VALUES (1, 'Paperback'),
                                               (2, 'Hard back');
|]

testFormat :: Int -> Text -> LoadedEntity EditionFormat
testFormat fid name = Entity { entityRef = Ref (toSql fid)
                             , entityInfo = EditionFormat { editionFormatName = name } }

tests :: [Test]
tests = [ testCase "allEditionFormats" $ databaseTest test_editionFormat_allEditionFormats
        , testCase "getByPk" $ databaseTest test_editionFormat_getByPk
        ]
