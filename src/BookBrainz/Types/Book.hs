-- | Definition of a book.
module BookBrainz.Types.Book
  ( Book(..)
  ) where

import Data.Text (Text)

--------------------------------------------------------------------------------
{-| A book is an abstract concept, and does not have a physical representation.
People own 'BookBrainz.Types.Edition.Edition's of books, of which there
can be multiple versions (reprints, translations, etc.). A book is an
abstraction of this set of editions, and is what people commonly talk about in
discussions. -}
data Book = Book
    { bookName :: Text  -- ^ The name of the book.
    } deriving Show
