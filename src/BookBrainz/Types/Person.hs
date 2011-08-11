-- | Definition of a person.
module BookBrainz.Types.Person
  ( Person(..)
  ) where

import Data.Text (Text)

--------------------------------------------------------------------------------
{-| A person involved with a book, be it author, editor, illustrator,
etc. -}
data Person = Person
    { -- | The name of the person.
      personName :: Text
    } deriving Show

