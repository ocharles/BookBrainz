{-# LANGUAGE TemplateHaskell #-}

module BookBrainz.Lenses where

import BookBrainz.Types
import Data.Record.Label

$(mkLabels [''Book])
