{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Functions for working with 'BookBrainz.Types.Edition.Edition' entities.
module BookBrainz.Model.Edition
       ( -- * Working With Editions
         findBookEditions
       ) where

import Data.Traversable                   (traverse)

import Data.Convertible                   (Convertible, safeConvert)
import Database.HDBC                      (toSql, fromSql, SqlValue)

import BookBrainz.Model.Role              (copyRoles)
import BookBrainz.Types
import BrainzStem.Database                (queryOne, safeQueryOne, (!)
                                          ,HasDatabase, query)
import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))

instance Convertible Isbn SqlValue where
  safeConvert = Right . toSql . show

instance Convertible SqlValue Isbn where
  safeConvert = Right . read . fromSql

instance GenericallyVersioned Edition where
  versioningConfig = VersionConfig { cfgView = "edition"
                                   , cfgIdCol = "edition_id"
                                   , cfgConcept = "edition"
                                   , cfgTree = "edition_tree"
                                   , cfgBbid = "edition_bbid"
                                   , cfgRevision = "edition_revision"
                                   , cfgBranch = "edition_branch"                                                                      
                                   }

  fromViewRow row =
    CoreEntity { bbid = row ! "bbid"
               , coreEntityRevision = row ! "revision"
               , coreEntityTree = row ! "edition_tree_id"
               , coreEntityConcept = row ! "edition_id"
               , coreEntityInfo = Edition { editionName = row ! "name"
                                          , editionFormat = row ! "format"
                                          , editionBook = row ! "book_id"
                                          , editionYear = row ! "year"
                                          , editionPublisher = row ! "publisher_id"
                                          , editionCountry = row ! "country_iso_code"
                                          , editionLanguage = row ! "language_iso_code"
                                          , editionIsbn = row ! "isbn"
                                          , editionIndex = row ! "edition_index"
                                          }
               }

  newTree baseTree pubData = do
    versionId <- findOrInsertVersion
    newTreeId <- fromSql `fmap` queryOne insertTreeSql [ versionId
                                                       , toSql $ editionBook pubData ]
    traverse (\tree -> copyRoles tree newTreeId) baseTree
    return newTreeId
    where
      findOrInsertVersion = do
        foundId <- findVersion
        case foundId of
          Just id' -> return id'
          Nothing -> newVersion
      insertTreeSql = unlines [ "INSERT INTO bookbrainz_v.edition_tree"
                              , "(version, book_id) VALUES (?, ?)"
                              , "RETURNING edition_tree_id"
                              ]
      findVersion =
        let findSql = unlines [ "SELECT version"
                              , "FROM bookbrainz_v.edition_v"
                              , "WHERE name = ?"
                              ]
        in safeQueryOne findSql [ toSql $ editionName pubData ]
      newVersion =
        let insertSql = unlines [ "INSERT INTO bookbrainz_v.edition_v"
                                , "(name) VALUES (?)"
                                , "RETURNING version"
                                ]
        in queryOne insertSql [ toSql $ editionName pubData ]

--------------------------------------------------------------------------------
-- | Find all editions of a specific 'Book'.
-- The book must be a 'LoadedCoreEntity', ensuring it exists in the database.
findBookEditions :: HasDatabase m
                 => Ref (Concept Book)
                 -- ^ The book to find editions of.
                 -> m [LoadedCoreEntity Edition]
                 -- ^ A (possibly empty) list of editions.
findBookEditions b = do
  results <- query selectQuery [ toSql b ]
  return $ fromViewRow `map` results
  where selectQuery = unlines [ "SELECT * "
                              , "FROM edition"
                              , "WHERE book_id = ?"
                              , "ORDER BY year, edition_index NULLS LAST"
                              ]
