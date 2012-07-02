{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Language (tests) where

import Test.BrainzStem

import BookBrainz.Model.Language
import BookBrainz.Types
import BrainzStem.Model

test_language_allLanguages :: DatabaseContext ()
test_language_allLanguages = do
  countries <- allLanguages
  liftIO $ do
    assertBool "Expected to find some countries" (length countries == 1)
    countries !! 0 @?= testLanguage

test_language_getByPk :: DatabaseContext ()
test_language_getByPk = do
  language <- getByPk $ Ref (toSql ("eng" :: String))
  liftIO $ language  @?= testLanguage

testLanguage = Entity { entityRef = Ref (toSql ("eng" :: String))
                     , entityInfo = Language { languageName = "English"
                                             , languageIsoCode = "eng" } }

tests :: [Test]
tests = [ testCase "allLanguages" $ databaseTest test_language_allLanguages
        , testCase "getByPk" $ databaseTest test_language_getByPk
        ]
