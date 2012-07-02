{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Country (tests) where

import Test.BrainzStem

import BookBrainz.Model.Country
import BookBrainz.Types
import BrainzStem.Model

test_country_allCountries :: DatabaseContext ()
test_country_allCountries = do
  countries <- allCountries
  liftIO $ do
    assertBool "Expected to find some countries" (length countries == 1)
    countries !! 0 @?= testCountry

test_country_getByPk :: DatabaseContext ()
test_country_getByPk = do
  country <- getByPk $ Ref (toSql ("GB" :: String))
  liftIO $ country  @?= testCountry

testCountry = Entity { entityRef = Ref (toSql ("GB" :: String))
                     , entityInfo = Country { countryName = "United Kingdom"
                                            , countryIsoCode = "GB" } }

tests :: [Test]
tests = [ testCase "allCountries" $ databaseTest test_country_allCountries
        , testCase "getByPk" $ databaseTest test_country_getByPk
        ]
