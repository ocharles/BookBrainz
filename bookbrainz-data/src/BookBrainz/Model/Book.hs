{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for working with 'BookBrainz.Types.Book.Book' entities.
module BookBrainz.Model.Book
       ( -- * Working With Books
         listAllBooks
       ) where

import Snap.Snaplet.PostgresqlSimple (HasPostgres, query_)

import BookBrainz.Schema ()
import BookBrainz.Types (LoadedCoreEntity (..), Book(..))

--------------------------------------------------------------------------------
-- | List the latest version of all known books.
listAllBooks :: (Functor m, HasPostgres m)
             => m [LoadedCoreEntity Book]
listAllBooks = query_ "SELECT * FROM canonical_book"
