module BookBrainz.Types.Book
  ( Book(..)
  ) where

import BookBrainz.Types.AuthorCredit
import BrainzStem.Types
import Data.Text (Text)

data Book = Book
            { bookName :: Text
            , bookAuthorCredit :: Ref AuthorCredit
            }
          deriving Show
