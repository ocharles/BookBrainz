module BookBrainz.Types.AuthorCredit
       ( AuthorCredit(..)
       ) where

import BookBrainz.Types.Newtypes (AuthorCreditId)
import BookBrainz.Types.Person
import Data.Text (Text)

data AuthorCredit = AuthorCredit
                    { authorCreditId :: AuthorCreditId
                    , authorCredits :: [Credit]
                    }
                  | AuthorCreditReference
                    { authorCreditId :: AuthorCreditId
                    }
                  deriving Show

data Credit = Credit { creditedName :: Text
                     , creditedAuthor :: Person
                     , creditedJoinPhrase :: Text
                     , creditedPosition :: Int
                     }
              deriving Show
