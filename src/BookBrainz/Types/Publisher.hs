-- | Definition of a publisher.
module BookBrainz.Types.Publisher
  ( Publisher(..)
  ) where

import Data.Text (Text)

--------------------------------------------------------------------------------
-- | An organization or company which publishes books.
data Publisher = Publisher
    { -- | The name of the publisher.
      publisherName :: Text
    } deriving Show
