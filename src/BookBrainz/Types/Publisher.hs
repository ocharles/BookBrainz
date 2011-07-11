module BookBrainz.Types.Publisher
  ( Publisher(..)
  ) where

import BookBrainz.Types.Newtypes (PublisherId)
import Data.Text (Text)
import Data.UUID (UUID)

data Publisher = Publisher
                 { publisherName :: Text
                 , publisherId :: PublisherId
                 , publisherGid :: UUID
                 }
               | PublisherReference
                 { publisherId :: PublisherId
                 } deriving Show
