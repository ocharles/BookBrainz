module BookBrainz.Controller.Book
       ( bookResource
       ) where

import BookBrainz.Controller (output)
import BookBrainz.Model
import BookBrainz.Model.Book
import BookBrainz.Types.MVC (Controller)
import BookBrainz.Types.Newtypes
import BookBrainz.View.Book (showBook)
import Data.Maybe (fromJust)

bookResource :: Controller ()
bookResource = do
  book <- model $ getBook $ BookId 1
  output $ showBook $ fromJust book
