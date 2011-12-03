{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Country (tests) where

import Test.BrainzStem
import Test.BrainzStem.Gen
import Test.BrainzStem.Model
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, suchThat, listOf1)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Applicative
import Database.HDBC (toSql)
import qualified Data.Text as T
import qualified Snap.Snaplet.Hdbc as HDBC

import BookBrainz.Model.Country
import BookBrainz.Types

instance Arbitrary Country where
  arbitrary = Country <$> (T.pack <$> name) <*> name `suchThat` (not . null)

instance Arbitrary (InDB Country LoadedEntity) where
  arbitrary = do
    c <- arbitrary
    return $ InDB c (insertCountry c >> (return $ toEnt c))
    where insertCountry c =
            HDBC.run "INSERT INTO country (iso_code, name) VALUES (?, ?)"
                     [ toSql $ countryIsoCode c
                     , toSql $ countryName c ]
          toEnt l = Entity { entityRef = Ref (toSql $ countryIsoCode l)
                           , entityInfo = l }

test_country_allCountries :: DatabaseContext ()
test_country_allCountries = do
  countries <- allCountries
  liftIO $ do
    assertBool "Expected to find some countries" (length countries == 1)
    countries !! 0 @?= testCountry

testCountry = Entity { entityRef = Ref (toSql ("GB" :: String))
                     , entityInfo = Country { countryName = "United Kingdom"
                                            , countryIsoCode = "GB" } }

tests :: [Test]
tests = [ testCase "allCountries" $ databaseTest test_country_allCountries
        , testProperty "getByPk" $ prop_getByPk
            (setBy (countryIsoCode . entity) <$> listOf1 arbitrary
              :: Gen [InDB Country LoadedEntity])
        ]
