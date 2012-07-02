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
                       ) where

import Test.HUnit hiding (Test)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad.IO.Class (liftIO)
import Snap.Snaplet.PostgresqlSimple

import BrainzStem.Database (openConnection, runDatabase, DatabaseContext)

type DatabaseTest = DatabaseContext ()

databaseTest :: DatabaseTest -> IO ()
databaseTest action = do
  db <- openConnection "bookbrainz_test" "bookbrainz"
  runDatabase db $ action >> rollback
