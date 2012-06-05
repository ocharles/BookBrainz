{-# LANGUAGE TypeFamilies #-}

module BrainzStem.Types.Internal where

import BrainzStem.Types

data instance Ref Editor = EditorRef Int
