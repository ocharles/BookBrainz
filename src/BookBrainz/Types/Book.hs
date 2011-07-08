module BookBrainz.Types.Book
  ( Book(..)
  ) where

import BookBrainz.Types.Newtypes (BookId)
import Data.Text (Text)
import Data.UUID (UUID)

data Book = Book
            { bookName :: Text
            , bookGid :: UUID
            , bookId :: BookId
            }
          | BookReference
            { bookId :: BookId
            } deriving Show
