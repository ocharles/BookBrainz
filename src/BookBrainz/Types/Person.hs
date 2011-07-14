module BookBrainz.Types.Person
  ( Person(..)
  ) where

import Data.Text (Text)

data Person = Person
              { personName :: Text
              }
            deriving Show

