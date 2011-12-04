module Test.BrainzStem.Gen (name, str) where

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, suchThat)
import qualified Data.Text as T

import Data.Char (isAlphaNum)

name :: Gen T.Text
name = T.pack `fmap` str

str :: Gen String
str = (arbitrary `suchThat` ('\NUL' `notElem`)) `suchThat` (all isAlphaNum)
