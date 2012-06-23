{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module BookBrainz.Search where

import           Control.Applicative

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Aeson             ((.=), ToJSON(..), FromJSON(..), object
                                        ,(.:), Value(..))
import qualified Data.Text              as T
import           Data.Aeson.Types       (typeMismatch)
import           Data.Copointed         (copoint)
import           Data.HashMap.Strict    (union)
import qualified Search.ElasticSearch   as ES
import           Search.ElasticSearch   (Document(..), DocumentType(..)
                                        ,localServer, Index, indexDocument)
import           Snap.Snaplet.PostgresqlSimple (HasPostgres)

import           BookBrainz.Model.Book ()
import           BookBrainz.Model.Edition ()
import           BookBrainz.Model.Role (findRoles)
import           BookBrainz.Types       as BB

--------------------------------------------------------------------------------
-- | The types of searches that are possible.
data SearchType = Book | Edition | Person | Publisher

--------------------------------------------------------------------------------
-- | A 'BB.Book' is searchable by it's name, and all 'BB.Role's.
data SearchableBook = SearchableBook
    { bookResult  :: LoadedCoreEntity BB.Book
    , bookRoles :: [ LoadedEntity BB.Role :. LoadedCoreEntity BB.Person ]
    }

instance Document SearchableBook where
  documentKey = T.pack . show . bbid . bookResult
  documentType = DocumentType "book"

instance ToJSON SearchableBook where
  toJSON (SearchableBook book roles) = toJSON book
                                       `unionObject`
                                       object [ "roles" .= toJSON roles ]

instance ToJSON Book where
  toJSON book = object [ "name" .= bookName book ]

instance ToJSON (LoadedEntity Role :. LoadedCoreEntity Person) where
  toJSON (role :. person) = object [ "role" .= role
                                 , "person" .= person
                                 ]

instance FromJSON SearchableBook where
  parseJSON json@(Object o) =
      SearchableBook <$> parseJSON json
                     <*> (o .: "roles" >>= mapM parseRole)
    where parseRole r = (:.) <$> r .: "role"
                             <*> r .: "person"
  parseJSON v = typeMismatch "SearchableBook" v

instance FromJSON BB.Book where
  parseJSON (Object b) = BB.Book <$> b .: "name"
  parseJSON v = typeMismatch "Book" v

instance ToJSON BB.Role where
  toJSON role = object [ "role" .= roleName role ]

instance FromJSON BB.Role where
  parseJSON (String r) = return $ BB.Role r
  parseJSON v = typeMismatch "Role" v

--------------------------------------------------------------------------------
-- | A 'BB.Edition' is searchable by its info and its roles.
data SearchableEdition = SearchableEdition
    { editionResult :: LoadedCoreEntity BB.Edition
    , editionRoles :: [ LoadedEntity BB.Role :. LoadedCoreEntity BB.Person ]
    }

instance Document SearchableEdition where
  documentKey = T.pack . show . bbid . editionResult
  documentType = DocumentType "edition"

instance ToJSON SearchableEdition where
  toJSON (SearchableEdition edition roles) = toJSON edition
                                             `unionObject`
                                             object [ "roles" .= toJSON roles ]

instance ToJSON BB.Edition where
  toJSON edition = object [ "name" .= editionName edition
                          , "book" .= editionBook edition
                          , "year" .= editionYear edition
                          , "publisher" .= editionPublisher edition
                          , "country" .= editionCountry edition
                          , "language" .= editionLanguage edition
                          , "isbn" .= editionIsbn edition
                          , "index" .= editionIndex edition
                          , "format" .= editionFormat edition
                          ]

instance FromJSON SearchableEdition where
  parseJSON json@(Object o) =
      SearchableEdition <$> parseJSON json
                        <*> (o .: "roles" >>= mapM parseRole)
    where parseRole r = (:.) <$> r .: "role"
                             <*> r .: "person"
  parseJSON v = typeMismatch "SearchableEdition" v

instance FromJSON BB.Edition where
  parseJSON json = case json of
    Object o -> BB.Edition <$> o .: "name"
                           <*> o .: "book"
                           <*> o .: "year"
                           <*> o .: "publisher"
                           <*> o .: "country"
                           <*> o .: "language"
                           <*> o .: "isbn"
                           <*> o .: "index"
                           <*> o .: "format"
    _ -> typeMismatch "SearchableEdition" json

--------------------------------------------------------------------------------
-- | A 'BB.Person' is searchable by it's name.
data SearchablePerson = SearchablePerson
    { personResult  :: LoadedCoreEntity BB.Person
    }

instance Document SearchablePerson where
  documentKey = T.pack . show . bbid . personResult
  documentType = DocumentType "person"

instance ToJSON SearchablePerson where
  toJSON (SearchablePerson person) = toJSON person

instance ToJSON Person where
  toJSON person = object [ "name" .= personName person ]

instance FromJSON SearchablePerson where
  parseJSON json = case json of
    Object _ -> SearchablePerson <$> parseJSON json
    _ -> typeMismatch "SearchablePerson" json

instance FromJSON BB.Person where
  parseJSON (Object b) = BB.Person <$> b .: "name"
  parseJSON v = typeMismatch "Person" v

--------------------------------------------------------------------------------
-- | A 'BB.Publisher' is searchable by it's name.
data SearchablePublisher = SearchablePublisher
    { publisherResult  :: LoadedCoreEntity BB.Publisher
    }

instance Document SearchablePublisher where
  documentKey = T.pack . show . bbid . publisherResult
  documentType = DocumentType "publisher"

instance ToJSON SearchablePublisher where
  toJSON (SearchablePublisher publisher) = toJSON publisher

instance ToJSON Publisher where
  toJSON publisher = object [ "name" .= publisherName publisher ]

instance FromJSON SearchablePublisher where
  parseJSON json = case json of
    Object _ -> SearchablePublisher <$> parseJSON json
    _ -> typeMismatch "SearchablePublisher" json

instance FromJSON BB.Publisher where
  parseJSON (Object b) = BB.Publisher <$> b .: "name"
  parseJSON v = typeMismatch "Publisher" v

--------------------------------------------------------------------------------
-- | Search for 'BB.Book's, given a query.
searchBooks :: (MonadIO m) => T.Text -> m (ES.SearchResults SearchableBook)
searchBooks = search BookBrainz.Search.Book

--------------------------------------------------------------------------------
-- | Search for 'BB.Edition's, given a query.
searchEditions :: (MonadIO m) => T.Text -> m (ES.SearchResults SearchableEdition)
searchEditions = search BookBrainz.Search.Edition

--------------------------------------------------------------------------------
-- | Search for 'BB.Person's, given a query.
searchPersons :: (MonadIO m) => T.Text -> m (ES.SearchResults SearchablePerson)
searchPersons = search BookBrainz.Search.Person

--------------------------------------------------------------------------------
-- | Search for 'BB.Publisher's, given a query.
searchPublishers :: (MonadIO m) => T.Text -> m (ES.SearchResults SearchablePublisher)
searchPublishers = search BookBrainz.Search.Publisher

--------------------------------------------------------------------------------
-- | Run a search for a given type of entity.
search :: (Document d, MonadIO m)
       => SearchType  -- ^ The type of entities to search for
       -> T.Text
       -> m (ES.SearchResults d)
search t = liftIO . ES.search localServer (typeToIndex t) 0

unionObject :: Value -> Value -> Value
unionObject (Object a) (Object b) = Object (a `union` b)
unionObject _ _ = error "unionObject can only be called with 2 Objects"

instance ToJSON (BBID a) where
  toJSON = toJSON . show

instance FromJSON (BBID a) where
  parseJSON (String s) =
    maybe (fail "Couldnt parse UUID") return (parseBbid $ T.unpack s)
  parseJSON v = typeMismatch "UUID" v

instance ( FromJSON ent, FromJSON (Ref (Concept ent)), FromJSON (Ref (Revision ent))
         , FromJSON (Ref (Tree ent)))
    => FromJSON (LoadedCoreEntity ent) where
  parseJSON json@(Object o) = CoreEntity <$> o .: "bbid"
                                         <*> o .: "_revision"
                                         <*> o .: "_tree"
                                         <*> parseJSON json
                                         <*> o .: "_concept"
  parseJSON v = typeMismatch "LoadedCoreEntity" v

instance ( ToJSON ent, ToJSON (Ref (Concept ent)), ToJSON (Ref (Revision ent))
         , ToJSON (Ref (Tree ent)))
    => ToJSON (LoadedCoreEntity ent) where
  toJSON ent = object [ "bbid" .= bbid ent
                       , "_tree" .= coreEntityTree ent
                       , "_revision" .= coreEntityRevision ent
                       , "_concept" .= coreEntityConcept ent
                       ]
               `unionObject`
               toJSON (copoint ent)

instance (ToJSON a, ToJSON (Ref a)) => ToJSON (LoadedEntity a) where
  toJSON loaded = object [ "_ref" .= entityRef loaded ]
                  `unionObject`
                  toJSON (copoint loaded)

instance (FromJSON entity, FromJSON (Ref entity))
    => FromJSON (LoadedEntity entity) where
  parseJSON json@(Object o) = Entity <$> parseJSON json
                                     <*> o .: "_ref"
  parseJSON v = typeMismatch "LoadedEntity" v

typeToIndex :: SearchType -> Index
typeToIndex BookBrainz.Search.Book = "book"
typeToIndex BookBrainz.Search.Edition = "edition"
typeToIndex BookBrainz.Search.Person = "person"
typeToIndex BookBrainz.Search.Publisher = "publisher"

instance FromJSON (Ref (Concept Book)) where parseJSON = intRef BookConceptRef
instance ToJSON (Ref (Concept Book)) where toJSON (BookConceptRef i) = toJSON i

instance FromJSON (Ref (Revision Book)) where parseJSON = intRef BookRevisionRef
instance ToJSON (Ref (Revision Book)) where toJSON (BookRevisionRef i) = toJSON i

instance FromJSON (Ref (Tree Book)) where parseJSON = intRef BookTreeRef
instance ToJSON (Ref (Tree Book)) where toJSON (BookTreeRef i) = toJSON i

instance FromJSON (Ref (Concept Edition)) where parseJSON = intRef EditionConceptRef
instance ToJSON (Ref (Concept Edition)) where toJSON (EditionConceptRef i) = toJSON i

instance FromJSON (Ref (Revision Edition)) where parseJSON = intRef EditionRevisionRef
instance ToJSON (Ref (Revision Edition)) where toJSON (EditionRevisionRef i) = toJSON i

instance FromJSON (Ref (Tree Edition)) where parseJSON = intRef EditionTreeRef
instance ToJSON (Ref (Tree Edition)) where toJSON (EditionTreeRef i) = toJSON i

instance FromJSON (Ref (Concept Person)) where parseJSON = intRef PersonConceptRef
instance ToJSON (Ref (Concept Person)) where toJSON (PersonConceptRef i) = toJSON i

instance FromJSON (Ref (Revision Person)) where parseJSON = intRef PersonRevisionRef
instance ToJSON (Ref (Revision Person)) where toJSON (PersonRevisionRef i) = toJSON i

instance FromJSON (Ref (Tree Person)) where parseJSON = intRef PersonTreeRef
instance ToJSON (Ref (Tree Person)) where toJSON (PersonTreeRef i) = toJSON i

instance FromJSON (Ref (Concept Publisher)) where parseJSON = intRef PublisherConceptRef
instance ToJSON (Ref (Concept Publisher)) where toJSON (PublisherConceptRef i) = toJSON i

instance FromJSON (Ref (Revision Publisher)) where parseJSON = intRef PublisherRevisionRef
instance ToJSON (Ref (Revision Publisher)) where toJSON (PublisherRevisionRef i) = toJSON i

instance FromJSON (Ref (Tree Publisher)) where parseJSON = intRef PublisherTreeRef
instance ToJSON (Ref (Tree Publisher)) where toJSON (PublisherTreeRef i) = toJSON i

instance FromJSON (Ref Role) where parseJSON = intRef RoleRef
instance ToJSON (Ref Role) where toJSON (RoleRef i) = toJSON i

instance FromJSON (Ref Country) where parseJSON = strRef CountryRef
instance ToJSON (Ref Country) where toJSON (CountryRef i) = toJSON i

instance FromJSON (Ref EditionFormat) where parseJSON = intRef EditionFormatRef
instance ToJSON (Ref EditionFormat) where toJSON (EditionFormatRef i) = toJSON i

instance FromJSON (Ref Language) where parseJSON = strRef LanguageRef
instance ToJSON (Ref Language) where toJSON (LanguageRef i) = toJSON i

instance FromJSON Isbn where
  parseJSON json = error "!!" -- read . T.unpack <$> parseJSON json

instance ToJSON Isbn where
  toJSON isbn = toJSON $ show isbn

intRef cons json = case json of
  Number _ -> cons <$> parseJSON json
  _ -> typeMismatch "Int" json

strRef cons json = case json of
  String _ -> cons <$> parseJSON json
  _ -> typeMismatch "String" json

--------------------------------------------------------------------------------
index' :: (Document d, MonadIO m) => SearchType -> d -> m ()
index' t d = liftIO $ indexDocument localServer (typeToIndex t) d

--------------------------------------------------------------------------------
-- | Given a 'Book' load and accompanying metadata and index.
indexBook :: (MonadIO m, HasPostgres m, Functor m)
          => LoadedCoreEntity Book
          -> m ()
indexBook b = findRoles (coreEntityTree b) >>= doIndex b
  where doIndex = (index' BookBrainz.Search.Book .) . SearchableBook

--------------------------------------------------------------------------------
-- | Given a 'Edition' load and accompanying metadata and index.
indexEdition :: (MonadIO m, HasPostgres m, Functor m)
          => LoadedCoreEntity Edition
          -> m ()
indexEdition b = findRoles (coreEntityTree b) >>= doIndex b
  where doIndex = (index' BookBrainz.Search.Edition .) . SearchableEdition

--------------------------------------------------------------------------------
-- | Given a 'Person' load accompanying metadata and index.
indexPerson :: (MonadIO m, HasPostgres m, Functor m)
            => LoadedCoreEntity Person
            -> m ()
indexPerson = (index' BookBrainz.Search.Person) . SearchablePerson

--------------------------------------------------------------------------------
-- | Given a 'Publisher' load accompanying metadata and index.
indexPublisher :: (MonadIO m, HasPostgres m, Functor m)
               => LoadedCoreEntity Publisher
               -> m ()
indexPublisher = (index' BookBrainz.Search.Publisher) . SearchablePublisher
