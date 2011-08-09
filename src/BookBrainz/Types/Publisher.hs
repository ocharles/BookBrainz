-- | The definition of a publisher
module BookBrainz.Types.Publisher
  ( Publisher(..)
  ) where

import Data.Text (Text)

--------------------------------------------------------------------------------
-- | A publisher
data Publisher = Publisher
    { -- | The name of the publisher
      publisherName :: Text
    } deriving Show
