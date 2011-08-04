module BookBrainz.Types.Book
  ( Book(..)
  ) where

import Data.Text (Text)

data Book = Book
            { bookName :: Text
            }
          deriving Show
