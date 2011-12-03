module Test.BrainzStem.Model (prop_getByPk) where

import Test.BrainzStem
import Test.QuickCheck (Property)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Monadic (monadicIO, pick, run, assert)

import BrainzStem.Model (Entity, getByPk)
import BrainzStem.Types

prop_getByPk :: (Entity a, Show a, Eq a)
             => Gen [InDB a LoadedEntity] -> Property
prop_getByPk gen = monadicIO $ do
  db <- pick gen
  b <- run $ databaseTest $ do
    entities <- initDb `mapM` db
    let e = head entities
    fetched <- getByPk (entityRef e)
    return $ e == fetched
  assert b
