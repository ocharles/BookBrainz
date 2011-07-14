module BookBrainz.Types.Person
  ( Person(..)
  ) where

import Data.Text (Text)
import Data.UUID (UUID)

data Person = Person
              { personName :: Text
              , personId :: Int
              , personGid :: UUID
              }
            deriving Show

