module BookBrainz.Types.Book
  ( Book(..)
  ) where

import BookBrainz.Types.AuthorCredit
import BookBrainz.Types.Newtypes (BookId)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.ByteString.Char8 (unpack)
import Data.UUID (UUID, fromString)

data Book = Book
            { bookName :: Text
            , bookGid :: UUID
            , bookId :: BookId
            , bookAuthorCredit :: AuthorCredit
            }
          | BookReference
            { bookId :: BookId
            } deriving Show
