{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.Search where

import Control.Applicative

import qualified Data.Text as T
import Data.Aeson           ((.=), ToJSON(..), FromJSON(..), object, (.:)
                            ,Value(..))
import Data.Aeson.Types     (typeMismatch)
import Data.Copointed       (copoint)
import Data.UUID            (fromString, toString, UUID)
import Search.ElasticSearch (Document(..), DocumentType(..), localServer
                            ,indexDocument)

import BookBrainz.Types

instance ToJSON UUID where
  toJSON = toJSON . toString

instance FromJSON UUID where
  parseJSON (String s) =
    maybe (fail "Couldnt parse UUID") return (fromString $ T.unpack s)
  parseJSON v = typeMismatch "UUID" v

--------------------------------------------------------------------------------
-- | A book is searchable by it's name, and all roles
data SearchableBook = SearchableBook
    { bookBook  :: LoadedCoreEntity Book
    }

instance Document SearchableBook where
  documentKey = toString . gid . bookBook
  documentType = DocumentType "book"

instance ToJSON SearchableBook where
  toJSON book = object [ "gid" .= gid (bookBook book)
                       , "_version" .= coreEntityVersion (bookBook book)
                       , "name" .= bookName (copoint $ bookBook book)
                       ]

instance FromJSON SearchableBook where
  parseJSON json@(Object o) =
      SearchableBook <$> parseBook
    where parseBook = CoreEntity <$> o .: "gid"
                                 <*> o .: "_version"
                                 <*> parseJSON json
  parseJSON v = typeMismatch "SearchableBook" v

instance FromJSON Book where
  parseJSON (Object b) = Book <$> b .: "name"
  parseJSON v = typeMismatch "Book" v


indexBook book = do
  indexDocument localServer "book" (SearchableBook book)
