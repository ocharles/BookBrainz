{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.EditionFormat (tests) where

import Test.BrainzStem
import Test.BrainzStem.Gen
import Test.BrainzStem.Model
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, listOf1)

import Control.Applicative
import Database.HDBC (toSql)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Snap.Snaplet.Hdbc as HDBC

import BrainzStem.Database
import BookBrainz.Model.EditionFormat
import BookBrainz.Types
import BrainzStem.Model

instance Arbitrary EditionFormat where
  arbitrary = EditionFormat <$> T.pack `fmap` name

instance Arbitrary (InDB EditionFormat LoadedEntity) where
  arbitrary = do
    format <- arbitrary
    return $ InDB format (insertFormat format >>= toEnt format)
    where insertFormat f =
            queryOne (unlines ["INSERT INTO edition_format (name) VALUES (?)"
                              ,"RETURNING id"])
                     [ toSql $ editionFormatName f ]
          toEnt r id = return $ Entity { entityRef = Ref id
                                       , entityInfo = r }

test_editionFormat_allEditionFormats :: DatabaseContext ()
test_editionFormat_allEditionFormats = do
  runRaw initialEnvironment
  formats <- allEditionFormats
  liftIO $ do
    assertBool "Expected to find some formats" (length formats == 2)
    formats !! 0 @?= testFormat 1 "Paperback"
    formats !! 1 @?= testFormat 2 "Hard back"

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
        , testProperty "getByPk" $ prop_getByPk
          ( listOf1 arbitrary :: Gen [InDB EditionFormat LoadedEntity] )
        ]
