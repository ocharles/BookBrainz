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

instance FromJSON entity => FromJSON (LoadedCoreEntity entity) where
  parseJSON json@(Object o) = CoreEntity <$> o .: "gid"
                                         <*> o .: "_version"
                                         <*> parseJSON json
  parseJSON v = typeMismatch "LoadedCoreEntity" v

instance (FromJSON entity) => FromJSON (LoadedEntity entity) where
  parseJSON json@(Object _) = Entity <$> parseJSON json
  parseJSON v = typeMismatch "LoadedEntity" v

--------------------------------------------------------------------------------
-- | A book is searchable by it's name, and all roles.
data SearchableBook = SearchableBook
    { bookBook  :: LoadedCoreEntity Book
    , bookRoles :: [(LoadedEntity Role, LoadedCoreEntity Person)]
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
      SearchableBook <$> parseJSON json
                     <*> (o .: "roles" >>= mapM parseRole)
    where parseRole r = (,) <$> r .: "role"
                            <*> r .: "person"
  parseJSON v = typeMismatch "SearchableBook" v

instance FromJSON Book where
  parseJSON (Object b) = Book <$> b .: "name"
  parseJSON v = typeMismatch "Book" v

instance FromJSON Role where
  parseJSON (String r) = return $ Role r
  parseJSON v = typeMismatch "Book" v

instance FromJSON Person where
  parseJSON (Object b) = Person <$> b .: "name"
  parseJSON v = typeMismatch "Book" v

index' :: Document d => String -> d -> IO ()
index' = indexDocument localServer

-- | Given a book and accompanying metadata, index the book.
indexBook :: LoadedCoreEntity Book
          -> [(LoadedEntity Role, LoadedCoreEntity Person)]
          -> IO ()
indexBook = (index' "book" .) . SearchableBook

