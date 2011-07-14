module BookBrainz.Types.Book
  ( Book(..)
  ) where

import BookBrainz.Types.AuthorCredit
import BookBrainz.Types.Ref
import Data.Text (Text)
import Data.UUID (UUID)

data Book = Book
            { bookName :: Text
            , bookGid :: UUID
            , bookId :: Int
            , bookAuthorCredit :: Ref AuthorCredit
            }
          deriving Show
