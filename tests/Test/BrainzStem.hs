module Test.BrainzStem ( DatabaseContext
                       , databaseTest
                       , DatabaseTest
                       , Test
                       , testCase
                       , testGroup
                       , testProperty

                       , (@?=)
                       , assertBool
                       , assertFailure
                       , liftIO

                       , str
                       , runRaw
                       , InDB(..)
                       , setBy
                       ) where

import Test.HUnit hiding (Test)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad.IO.Class (liftIO)
import Data.List (nubBy)
import Data.String.Interpolation
import Snap.Snaplet.Hdbc (rollback, runRaw)

import BrainzStem.Database (openConnection, runDatabase, DatabaseContext)

type DatabaseTest = DatabaseContext ()

databaseTest :: DatabaseContext a -> IO a
databaseTest action = do
  db <- openConnection "bookbrainz_test" "bookbrainz"
  runDatabase db $ do r <- action
                      rollback
                      return r

data InDB a c = InDB { entity :: a
                     , initDb :: DatabaseContext (c a) }

instance Show a => Show (InDB a c) where
  show (InDB e _) = show e

setBy :: Eq b => (a -> b) -> [a] -> [a]
setBy f = nubBy (\x y -> f x == f y)
