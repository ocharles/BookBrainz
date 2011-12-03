{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.BookBrainz.Language (tests) where

import Test.BrainzStem
import Test.BrainzStem.Gen
import Test.BrainzStem.Model

import Control.Applicative
import Control.Monad (void)
import Database.HDBC (toSql)
import qualified Data.Text as T

import BookBrainz.Model.Language
import BookBrainz.Types

import qualified Snap.Snaplet.Hdbc as HDBC

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, suchThat)
import Test.QuickCheck.Monadic (monadicIO, pick, run, assert)
import Test.Framework.Providers.QuickCheck2 (testProperty)

instance Arbitrary (DBState (LoadedEntity Language)) where
  arbitrary = do
    l <- Language <$> (T.pack <$> name) <*> name `suchThat` (not . null)
    return $ DBState (void $ insertLanguage l) (toEnt l)
    where insertLanguage l =
            HDBC.run "INSERT INTO language (iso_code, name) VALUES (?, ?)"
                     [ toSql $ languageIsoCode l
                     , toSql $ languageName l ]
          toEnt l = Entity { entityRef = Ref (toSql $ languageIsoCode l)
                           , entityInfo = l }

prop_allLanguages = monadicIO $ do
  states <- setBy (entityRef . entity) <$> pick arbitrary
  let languages = entity `map` states
  fetched <- run $ databaseTest $ initDb `mapM` states >> allLanguages
  assert $ languages `eqSet` fetched
  where eqSet a b = all (`elem` a) b && all (`elem` b) a

tests :: [Test]
tests = [ testProperty "allLanguages" prop_allLanguages
        , testProperty "getByPk" $ prop_getByPk
            (arbitrary :: Gen (DBState (LoadedEntity Language)))
        ]
