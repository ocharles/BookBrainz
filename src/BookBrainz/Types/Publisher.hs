module BookBrainz.Types.Publisher
  ( Publisher(..)
  ) where

import Data.Text (Text)

data Publisher = Publisher
                 { publisherName :: Text
                 , publisherId :: Int
                 }
               deriving Show
