module Test.BrainzStem.Arbitrary where

import Test.BrainzStem (InDB(..))
import Test.BrainzStem.Gen
import Test.QuickCheck (Arbitrary(..))

import Control.Applicative
import Database.HDBC (toSql)

import BrainzStem.Database
import BrainzStem.Types

instance Arbitrary Editor where
  arbitrary = Editor <$> name

instance Arbitrary (InDB Editor LoadedEntity) where
  arbitrary = do
    editor <- arbitrary
    return $ InDB editor (insertEditor editor >>= return . toEnt editor)
    where insertEditor e = do
            queryOne (unlines [ "INSERT INTO editor (name, password) VALUES (?, 'a')"
                              , "RETURNING editor_id" ])
                     [ toSql $ editorName e]
          toEnt editor eid = Entity { entityRef = Ref eid
                                    , entityInfo = editor }
