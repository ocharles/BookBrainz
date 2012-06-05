{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Functions for working with 'BookBrainz.Types.Edition.Edition' entities.
module BookBrainz.Model.Edition
       ( -- * Working With Editions
         findBookEditions
       ) where

import Control.Applicative ((<$>))
import Data.String (fromString)
import Database.PostgreSQL.Simple (Only(..))

import BookBrainz.Model.Role              (copyRoles, HasRoles(..), addRole', findRoles', copyRoles')
import BookBrainz.Schema ()
import BookBrainz.Types
import BrainzStem.Database                (queryOne, safeQueryOne)
import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))

import Snap.Snaplet.PostgresqlSimple (query, HasPostgres)

instance GenericallyVersioned Edition where
  versioningConfig = VersionConfig { cfgView = "edition"
                                   , cfgIdCol = "edition_id"
                                   , cfgConcept = "edition"
                                   , cfgTree = "edition_tree"
                                   , cfgBbid = "edition_bbid"
                                   , cfgRevision = "edition_revision"
                                   , cfgBranch = "edition_branch"
                                   }

  newTreeImpl pubData = do
    versionId <- findOrInsertVersion
    newTreeId <- fromOnly `fmap` queryOne insertTreeSql (versionId, editionBook pubData, editionPublisher pubData)
    --traverse (\tree -> copyRoles tree newTreeId) baseTree
    return newTreeId
    where
      findOrInsertVersion = do
        foundId <- findVersion
        case foundId of
          Just id' -> return id'
          Nothing -> newVersion
      insertTreeSql = fromString $ unlines [ "INSERT INTO bookbrainz_v.edition_tree"
                              , "(version, book_id, publisher_id) VALUES (?, ?, ?)"
                              , "RETURNING edition_tree_id"
                              ]
      findVersion =
        let findSql = fromString $ unlines [ "SELECT version"
                              , "FROM bookbrainz_v.edition_v"
                              , "WHERE name = ? AND year = ? AND country_iso_code = ?"
                              , "AND language_iso_code = ? AND isbn = ?"
                              , "AND format = ?"
                              ]
        in (fmap fromOnly) `fmap` safeQueryOne findSql
                                ( editionName pubData, editionYear pubData
                                , editionCountry pubData
                                , editionLanguage pubData
                                , editionIsbn pubData
                                , editionFormat pubData
                                )
      newVersion =
        let insertSql = fromString $ unlines [ "INSERT INTO bookbrainz_v.edition_v"
                                , "(name, year, country_iso_code, language_iso_code, isbn, format)"
                                , "VALUES (?, ?, ?, ?, ?, ?)"
                                , "RETURNING version"
                                ]
        in (\x -> x `asTypeOf` (undefined :: Int)) <$>
               (fmap fromOnly $ queryOne insertSql
                              ( editionName pubData, editionYear pubData
                              , editionCountry pubData
                              , editionLanguage pubData
                              , editionIsbn pubData
                              , editionFormat pubData
                              ))

--------------------------------------------------------------------------------
-- | Find all editions of a specific 'Book'.
-- The book must be a 'LoadedCoreEntity', ensuring it exists in the database.
findBookEditions :: (Functor m, HasPostgres m)
                 => Ref (Concept Book)
                 -- ^ The book to find editions of.
                 -> m [LoadedCoreEntity Edition]
                 -- ^ A (possibly empty) list of editions.
findBookEditions b = query selectQuery (Only b)
  where selectQuery = fromString $ unlines [ "SELECT * "
                              , "FROM edition"
                              , "WHERE book_id = ?"
                              , "ORDER BY year, edition_index NULLS LAST"
                              ]

instance HasRoles Edition where
  findRoles = findRoles' "edition"
  copyRoles = copyRoles' "edition"
  addRole = addRole' "edition"
