-- | Definition of a person
module BookBrainz.Types.Person
  ( Person(..)
  ) where

import Data.Text (Text)

--------------------------------------------------------------------------------
{-| A person involved within a book, be it author, publishing, illustration,
etc -}
data Person = Person
    { -- | The name of this person
      personName :: Text
    } deriving Show

