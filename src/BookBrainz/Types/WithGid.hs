module BookBrainz.Types.WithGid where

import Data.Copointed
import Data.UUID

data WithGid a = WithGid { gid :: UUID, info :: a }
               deriving Show

instance Copointed WithGid where
  copoint = info
