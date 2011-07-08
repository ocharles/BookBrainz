module BookBrainz.Types.Person
  ( Person(..)
  ) where

import BookBrainz.Types.Newtypes (PersonId)
import Data.Text (Text)
import Data.UUID (UUID)

data Person = Person
              { personName :: Text
              , personId :: PersonId
              , personGid :: UUID
              }
            | PersonReference
              { personId :: PersonId
              }
            deriving Show

