{-# LANGUAGE TypeOperators #-}
module BookBrainz.Search.Indexer where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Search.ElasticSearch   (Document(..), DocumentType(..)
                              ,localServer, indexDocument, Index)

import BookBrainz.Search
import BookBrainz.Types

index' :: (Document d, MonadIO m) => SearchType -> d -> m ()
index' t d = liftIO $ indexDocument localServer (typeToIndex t) d

--------------------------------------------------------------------------------
-- | Given a book and accompanying metadata, index the book.
indexBook :: (MonadIO m)
          => LoadedCoreEntity Book
          -> [LoadedEntity Role :. LoadedCoreEntity Person]
          -> m ()
indexBook = (index' BookBrainz.Search.Book .) . SearchableBook
