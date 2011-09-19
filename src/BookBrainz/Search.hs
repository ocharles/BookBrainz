{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module BookBrainz.Search where

import           Control.Applicative

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Aeson             ((.=), ToJSON(..), FromJSON(..), object
                                        ,(.:), Value(..))
import qualified Data.Text              as T
import           Database.HDBC          (toSql, fromSql)
import           Data.Aeson.Types       (typeMismatch)
import           Data.Copointed         (copoint)
import           Data.Map               (union)
import           Data.UUID              (fromString, toString, UUID)
import qualified Search.ElasticSearch   as ES
import           Search.ElasticSearch   (Document(..), DocumentType(..)
                                        ,localServer, indexDocument, Index)

import           BookBrainz.Types       as BB

--------------------------------------------------------------------------------
-- | The types of searches that are possible.
data SearchType = Book

--------------------------------------------------------------------------------
-- | A book is searchable by it's name, and all roles.
data SearchableBook = SearchableBook
    { bookResult  :: LoadedCoreEntity BB.Book
    , bookRoles :: [(LoadedEntity BB.Role, LoadedCoreEntity BB.Person)]
    }

instance Document SearchableBook where
  documentKey = toString . gid . bookResult
  documentType = DocumentType "book"

instance ToJSON SearchableBook where
  toJSON (SearchableBook book roles) = toJSON book
                                       `unionObject`
                                       object [ "roles" .= toJSON roles ]

instance ToJSON Book where
  toJSON book = object [ "name" .= bookName book ]

instance ToJSON Person where
  toJSON person = object [ "name" .= personName person ]

instance ToJSON (LoadedEntity Role, LoadedCoreEntity Person) where
  toJSON (role, person) = object [ "role" .= roleName (copoint role)
                                 , "person" .= person
                                 ]

instance FromJSON SearchableBook where
  parseJSON json@(Object o) =
      SearchableBook <$> parseJSON json
                     <*> (o .: "roles" >>= mapM parseRole)
    where parseRole r = (,) <$> r .: "role"
                            <*> r .: "person"
  parseJSON v = typeMismatch "SearchableBook" v

instance FromJSON BB.Book where
  parseJSON (Object b) = BB.Book <$> b .: "name"
  parseJSON v = typeMismatch "Book" v

instance FromJSON BB.Role where
  parseJSON (String r) = return $ BB.Role r
  parseJSON v = typeMismatch "Role" v

instance FromJSON BB.Person where
  parseJSON (Object b) = BB.Person <$> b .: "name"
  parseJSON v = typeMismatch "Person" v

index' :: (Document d, MonadIO m) => SearchType -> d -> m ()
index' t d = liftIO $ indexDocument localServer (typeToIndex t) d

--------------------------------------------------------------------------------
-- | Given a book and accompanying metadata, index the book.
indexBook :: (MonadIO m)
          => LoadedCoreEntity BB.Book
          -> [(LoadedEntity BB.Role, LoadedCoreEntity BB.Person)]
          -> m ()
indexBook = (index' BookBrainz.Search.Book .) . SearchableBook

--------------------------------------------------------------------------------
-- | Search for books, given a query.
searchBooks :: (MonadIO m) => T.Text -> m (ES.SearchResults SearchableBook)
searchBooks = search BookBrainz.Search.Book

--------------------------------------------------------------------------------
-- | Run a search for a given type of entity.
search :: (Document d, MonadIO m)
       => SearchType  -- ^ The type of entities to search for
       -> T.Text
       -> m (ES.SearchResults d)
search t = liftIO . ES.search localServer (typeToIndex t)

unionObject :: Value -> Value -> Value
unionObject (Object a) (Object b) = Object (a `union` b)
unionObject _ _ = error "unionObject can only be called with 2 Objects"

instance ToJSON UUID where
  toJSON = toJSON . toString

instance FromJSON UUID where
  parseJSON (String s) =
    maybe (fail "Couldnt parse UUID") return (fromString $ T.unpack s)
  parseJSON v = typeMismatch "UUID" v

instance FromJSON entity => FromJSON (LoadedCoreEntity entity) where
  parseJSON json@(Object o) = CoreEntity <$> o .: "gid"
                                         <*> o .: "_revision"
                                         <*> o .: "_version"
                                         <*> parseJSON json
                                         <*> o .: "_id"
  parseJSON v = typeMismatch "LoadedCoreEntity" v

instance ToJSON ent => ToJSON (LoadedCoreEntity ent) where
  toJSON ent = object [ "gid" .= gid ent
                       , "_tree" .= coreEntityTree ent
                       , "_revision" .= coreEntityRevision ent
                       , "_id" .= coreEntityId ent
                       ]
               `unionObject`
               toJSON (copoint ent)

instance (FromJSON entity) => FromJSON (LoadedEntity entity) where
  parseJSON json = Entity <$> parseJSON json

instance ToJSON (Ref a) where
  toJSON ref = toJSON (fromSql $ rkey ref :: String)

instance FromJSON (Ref a) where
  parseJSON (String s) = return (Ref $ toSql $ T.unpack s)

typeToIndex :: SearchType -> Index
typeToIndex BookBrainz.Search.Book = "book"
