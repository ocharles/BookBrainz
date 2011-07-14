module BookBrainz.Types.AuthorCredit
       ( AuthorCredit(..)
       , Credit(..)
       ) where

import BookBrainz.Types.Person
import BrainzStem.Types
import Data.Text (Text)

data AuthorCredit = AuthorCredit
                    { authorCredits :: [Credit]
                    }
                  deriving Show

data Credit = Credit { creditedName :: Text
                     , creditedAuthor :: LoadedCoreEntity Person
                     , creditedJoinPhrase :: Text
                     , creditedPosition :: Int
                     }
              deriving Show
