module Test.BrainzStem.Model (prop_getByPk) where

import Test.BrainzStem (setBy, entity, initDb, databaseTest)
import Test.QuickCheck.Gen (elements, Gen, suchThat, sized, listOf, listOf1)
import Test.QuickCheck.Monadic (monadicIO, pick, run, assert)

import Control.Applicative

import BrainzStem.Model (getByPk)
import BrainzStem.Types

prop_getByPk gen = monadicIO $ do
  states <- setBy (entityRef . entity) <$> pick (listOf1 gen)
  let e = head $ entity `map` states
  fetched <- run $ databaseTest $ initDb `mapM` states >> getByPk (entityRef e)
  assert $ e == fetched
