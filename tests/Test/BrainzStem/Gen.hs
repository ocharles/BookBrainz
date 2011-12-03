module Test.BrainzStem.Gen (name) where

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, suchThat)

name :: Gen String
name = arbitrary `suchThat` ('\NUL' `notElem`)
