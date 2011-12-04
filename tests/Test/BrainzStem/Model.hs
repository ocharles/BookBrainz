module Test.BrainzStem.Model
    ( prop_getByPk
    , prop_create
    , prop_getByBbid
    ) where

import Test.BrainzStem
import Test.BrainzStem.Arbitrary ()
import Test.QuickCheck (Property)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Monadic (monadicIO, pick, run, assert)

import BrainzStem.Model (Entity, CoreEntity, getByPk, create, getByBbid)
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

prop_create :: (CoreEntity a, Show a, Eq a)
            => Gen a -> Property
prop_create versionGen = monadicIO $ do
  version <- pick versionGen
  dbEditor <- pick arbitrary
  b <- run $ databaseTest $ do
    editor <- initDb dbEditor
    createdEnt <- create version (entityRef editor)
    maybeFetchedEnt <- getByBbid $ bbid createdEnt
    case maybeFetchedEnt of
      Just fetchedEnt -> return $ fetchedEnt == createdEnt
      Nothing -> return False
  assert b

prop_getByBbid :: (CoreEntity a, Show a, Eq a)
               => Gen [InDB a LoadedCoreEntity] -> Property
prop_getByBbid gen = monadicIO $ do
  dbEnts <- pick gen
  b <- run $ databaseTest $ do
    ents <- initDb `mapM` dbEnts
    let expected = head ents
    fetched <- getByBbid $ bbid expected
    case fetched of
      Just f -> return $ f == expected
      Nothing -> return False
  assert b
