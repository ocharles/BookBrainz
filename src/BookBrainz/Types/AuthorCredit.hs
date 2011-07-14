module BookBrainz.Types.AuthorCredit
       ( AuthorCredit(..)
       , Credit(..)
       ) where

import BookBrainz.Types.Person
import BookBrainz.Types.WithGid
import Data.Text (Text)

data AuthorCredit = AuthorCredit
                    { authorCreditId :: Int
                    , authorCredits :: [Credit]
                    }
                  deriving Show

data Credit = Credit { creditedName :: Text
                     , creditedAuthor :: WithGid Person
                     , creditedJoinPhrase :: Text
                     , creditedPosition :: Int
                     }
              deriving Show
