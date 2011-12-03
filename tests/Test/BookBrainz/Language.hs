{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.BookBrainz.Language (tests) where

import Test.BrainzStem

import Control.Applicative
import Control.Monad (forM, void)
import Data.Char (isAlphaNum)
import Data.List (nubBy)
import Database.HDBC (toSql)
import Data.Text (Text)
import qualified Data.Text as T

import BookBrainz.Model.Language
import BookBrainz.Types
import BrainzStem.Model

import qualified Snap.Snaplet.Hdbc as HDBC

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements, Gen, suchThat, sized, listOf, listOf1)
import Test.QuickCheck.Monadic (monadicIO, pick, run, assert)
import Test.Framework.Providers.QuickCheck2 (testProperty)

data DBState a = DBState { initDb :: DatabaseContext ()
                         , entity :: a
                         }

instance Show a => Show (DBState a) where
  show (DBState _ e) = show e

setBy :: Eq b => (a -> b) -> [a] -> [a]
setBy f = nubBy (\x y -> f x == f y)

name :: Gen String
name = arbitrary `suchThat` ('\NUL' `notElem`)

-- Initialize a random database with languages, and keep track of what languages
-- are available.
instance Arbitrary (DBState (LoadedEntity Language)) where
  arbitrary = do
    l <- Language <$> (T.pack <$> name) <*> name `suchThat` (not . null)
    return $ DBState (void $ insertLanguage l) (toEnt l)
    where insertLanguage l = HDBC.run "INSERT INTO language (iso_code, name) VALUES (?, ?)"
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

prop_getByPk = monadicIO $ do
  states <- setBy (entityRef . entity) <$> pick (listOf1 arbitrary :: Gen [DBState (LoadedEntity Language)])
  let language = head $ entity `map` states
  fetched <- run $ databaseTest $ initDb `mapM` states >> getByPk (entityRef language)
  assert $ language == fetched

tests :: [Test]
tests = [ testProperty "allLanguages" prop_allLanguages
        , testProperty "getByPk" prop_getByPk
        ]
