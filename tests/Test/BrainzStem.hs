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
                       ) where

import Test.HUnit hiding (Test)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad.IO.Class (liftIO)
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
