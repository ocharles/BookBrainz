module BookBrainz.Types.Book
  ( Book(..)
  ) where

import BookBrainz.Types.AuthorCredit
import BookBrainz.Types.Ref
import Data.Text (Text)

data Book = Book
            { bookName :: Text
            , bookId :: Int
            , bookAuthorCredit :: Ref AuthorCredit
            }
          deriving Show
