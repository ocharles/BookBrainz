{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module BookBrainz.Schema where

import Control.Applicative ((<$>), (<*>), pure, Applicative(..))
import Data.ByteString.Char8 (pack, unpack)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.FromField
     (FromField(..), ResultError(..), returnError, typename)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(Escape))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))

import BookBrainz.Types
import BrainzStem.Types.Internal

--------------------------------------------------------------------------------
instance (ToRow h, ToRow t) => ToRow (h :. t) where
  toRow (h :. t) = toRow h ++ toRow t

--------------------------------------------------------------------------------
instance FromRow (LoadedEntity Editor) where
  fromRow = do
    name <- field
    id' <- field
    return Entity { entityInfo = Editor { editorName = name
                                        }
                  , entityRef = EditorRef id'
                  }

instance ToField (Ref Editor) where
  toField (EditorRef eid) = toField eid

instance FromField (Ref Editor) where
  fromField f v = EditorRef <$> fromField f v

--------------------------------------------------------------------------------
instance ToField (BBID a) where
  toField = Escape . pack . show

instance Typeable a => FromField (BBID a) where
  fromField f i
    | typename f /= "uuid" = returnError Incompatible f ""
    | otherwise            = case i of
        (Just bs) -> case parseBbid $ unpack bs of
          (Just bbid') -> pure bbid'
          Nothing -> returnError ConversionFailed f "Not a valid UUID"
        Nothing -> returnError UnexpectedNull f ""

--------------------------------------------------------------------------------
instance FromRow (LoadedCoreEntity Edition) where
  fromRow = do
    bbid' <- field
    e' <- edition
    treeId <- field
    revId <- field
    return CoreEntity { bbid = bbid'
                      , coreEntityRevision = revId
                      , coreEntityTree = treeId
                      , coreEntityInfo = e'
                      }
    where edition = Edition
            <$> field <*> field <*> field <*> field <*> field <*> field
            <*> field <*> field <*> field

--------------------------------------------------------------------------------
instance FromRow (LoadedCoreEntity Book) where
  fromRow = do
    bbid' <- field
    revId <- field
    treeId <- field
    name <- field
    return CoreEntity { bbid = bbid'
                      , coreEntityRevision = revId
                      , coreEntityTree = treeId
                      , coreEntityInfo = Book { bookName = name }
                      }

instance FromField (Ref (Revision Book)) where
  fromField f v = BookRevisionRef <$> fromField f v

instance FromField (Ref Book) where
  fromField f v = BookRef <$> fromField f v

instance FromField (Ref (Tree Book)) where
  fromField f v = BookTreeRef <$> fromField f v

instance ToField (Ref (Revision Book)) where
  toField (BookRevisionRef rid) = toField rid

instance ToField (Ref (Tree Book)) where
  toField (BookTreeRef rid) = toField rid

instance ToField (Ref Book) where
  toField (BookRef rid) = toField rid

--------------------------------------------------------------------------------
instance FromRow (LoadedEntity Country) where
  fromRow = do
    name <- field
    isoCode <- field
    return Entity { entityInfo = Country { countryName = name
                                         , countryIsoCode = isoCode
                                         }
                  , entityRef = CountryRef isoCode
                  }


instance ToField (Ref Country) where
  toField (CountryRef iso) = toField iso

instance FromField (Ref Country) where
  fromField f v = CountryRef <$> fromField f v

--------------------------------------------------------------------------------
instance FromRow (LoadedEntity EditionFormat) where
  fromRow = do
    name <- field
    efid <- field
    return Entity { entityInfo = EditionFormat { editionFormatName = name }
                  , entityRef = EditionFormatRef efid
                  }

instance ToField (Ref EditionFormat) where
  toField (EditionFormatRef iso) = toField iso

instance FromField (Ref EditionFormat) where
  fromField f v = EditionFormatRef <$> fromField f v

--------------------------------------------------------------------------------
instance FromField (Ref (Revision Edition)) where
  fromField f v = EditionRevisionRef <$> fromField f v

instance FromField (Ref Edition) where
  fromField f v = EditionRef <$> fromField f v

instance FromField (Ref (Tree Edition)) where
  fromField f v = EditionTreeRef <$> fromField f v

instance ToField (Ref (Revision Edition)) where
  toField (EditionRevisionRef rid) = toField rid

instance ToField (Ref (Tree Edition)) where
  toField (EditionTreeRef rid) = toField rid

instance ToField (Ref Edition) where
  toField (EditionRef rid) = toField rid

--------------------------------------------------------------------------------
instance FromRow (LoadedCoreEntity Person) where
  fromRow = do
    bbid' <- field
    name <- field
    treeId <- field
    revId <- field
    return CoreEntity { bbid = bbid'
                      , coreEntityRevision = revId
                      , coreEntityTree = treeId
                      , coreEntityInfo = Person { personName = name }
                      }
instance FromField (Ref (Revision Person)) where
  fromField f v = PersonRevisionRef <$> fromField f v

instance FromField (Ref Person) where
  fromField f v = PersonRef <$> fromField f v

instance FromField (Ref (Tree Person)) where
  fromField f v = PersonTreeRef <$> fromField f v

instance ToField (Ref (Revision Person)) where
  toField (PersonRevisionRef rid) = toField rid

instance ToField (Ref (Tree Person)) where
  toField (PersonTreeRef rid) = toField rid

instance ToField (Ref Person) where
  toField (PersonRef rid) = toField rid

--------------------------------------------------------------------------------
instance FromRow (LoadedEntity Role) where
  fromRow = do
    roleId <- field
    name <- field
    return Entity { entityInfo = Role { roleName = name }
                  , entityRef = RoleRef roleId
                  }

instance ToField (Ref Role) where
  toField (RoleRef rid) = toField rid

--------------------------------------------------------------------------------
instance FromRow (LoadedCoreEntity Publisher) where
  fromRow = do
    bbid' <- field
    name <- field
    treeId <- field
    revId <- field
    return CoreEntity { bbid = bbid'
                      , coreEntityRevision = revId
                      , coreEntityTree = treeId
                      , coreEntityInfo = Publisher { publisherName = name }
                      }

instance FromField (Ref (Revision Publisher)) where
  fromField f v = PublisherRevisionRef <$> fromField f v

instance FromField (Ref Publisher) where
  fromField f v = PublisherRef <$> fromField f v

instance FromField (Ref (Tree Publisher)) where
  fromField f v = PublisherTreeRef <$> fromField f v

instance ToField (Ref (Revision Publisher)) where
  toField (PublisherRevisionRef rid) = toField rid

instance ToField (Ref (Tree Publisher)) where
  toField (PublisherTreeRef rid) = toField rid

instance ToField (Ref Publisher) where
  toField (PublisherRef rid) = toField rid

--------------------------------------------------------------------------------
instance FromField Isbn where
  fromField _ (Just v) = (return . read . unpack) v
  fromField f Nothing = returnError UnexpectedNull f ""

instance ToField Isbn where
  toField = toField . show

--------------------------------------------------------------------------------
instance FromRow (LoadedEntity Language) where
  fromRow = do
    name <- field
    isoCode <- field
    return Entity { entityInfo =
                      Language { languageName = name
                               , languageIsoCode = isoCode
                               }
                  , entityRef = LanguageRef isoCode
                  }

instance FromField (Ref Language) where
  fromField f v = LanguageRef <$> fromField f v

instance ToField (Ref Language) where
  toField (LanguageRef lid) = toField lid

--------------------------------------------------------------------------------
instance (FromField (Ref (Tree a)), FromField (Ref (Revision a)))
    => FromRow (LoadedEntity (Revision a)) where
 fromRow = do
    revisionTree' <- field
    entityRef' <- field
    revTime <- field
    editorId <- field
    return $ Entity { entityInfo = Revision { revisionTree = revisionTree'
                                            , revisionTime = revTime
                                            , revisionEditor = editorId
                                            }
                    , entityRef = entityRef'
                    }
