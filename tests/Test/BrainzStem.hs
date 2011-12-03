module Test.BrainzStem ( DatabaseContext
                       , databaseTest
                       , DatabaseTest
                       , Test
                       , testCase
                       , testGroup

                       , (@?=)
                       , assertBool
                       , assertFailure
                       , liftIO

                       , str
                       , runRaw
                       , DBState(..)
                       , setBy
                       ) where

import Test.HUnit hiding (Test)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

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

data DBState a = DBState { initDb :: DatabaseContext ()
                         , entity :: a
                         }

instance Show a => Show (DBState a) where
  show (DBState _ e) = show e

setBy :: Eq b => (a -> b) -> [a] -> [a]
setBy f = nubBy (\x y -> f x == f y)
