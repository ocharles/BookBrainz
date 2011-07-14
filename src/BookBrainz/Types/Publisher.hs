module BookBrainz.Types.Publisher
  ( Publisher(..)
  ) where

import Data.Text (Text)
import Data.UUID (UUID)

data Publisher = Publisher
                 { publisherName :: Text
                 , publisherId :: Int
                 , publisherGid :: UUID
                 }
               deriving Show
