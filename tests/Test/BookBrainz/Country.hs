{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Country (tests) where

import Test.BrainzStem
import Test.BrainzStem.Gen
import Test.BrainzStem.Model
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, listOf1)
import Test.QuickCheck.Monadic (pick, assert, run, monadicIO)
import Test.QuickCheck.Property (Property)

import Control.Applicative
import Database.HDBC (toSql)
import qualified Snap.Snaplet.Hdbc as HDBC

import BookBrainz.Model.Country
import BookBrainz.Types

instance Arbitrary Country where
  arbitrary = Country <$> name <*> str

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

prop_allCountries :: Property
prop_allCountries = monadicIO $ do
  dbCountries <- pick (listOf1 arbitrary)
  b <- run $ databaseTest $ do
    expect <- initDb `mapM` dbCountries
    actual <- allCountries
    return $ expect == actual
  assert b

tests :: [Test]
tests = [ testProperty "allCountries" $ prop_allCountries
        , testProperty "getByPk" $ prop_getByPk
            (setBy (countryIsoCode . entity) <$> listOf1 arbitrary
              :: Gen [InDB Country LoadedEntity])
        ]
