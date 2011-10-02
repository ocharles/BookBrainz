{-# LANGUAGE FlexibleContexts #-}

-- | Functions for working with 'BookBrainz.Types.Edition.Edition' entities.
module BookBrainz.Model.Edition
       ( -- * Working With Editions
         findBookEditions
       ) where

import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))

import Database.HDBC                      (toSql, fromSql)

import BookBrainz.Types                   (Edition (..), Book)
import BrainzStem.Database                (queryOne, safeQueryOne, (!)
                                          ,HasDatabase, query)
import BrainzStem.Types                   (LoadedCoreEntity (..), Ref, Tree)

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
                                          , editionBarcode = row ! "barcode"
                                          , editionIndex = row ! "edition_index"
                                          }
               }

  findVersion pubData = fmap fromSql `fmap`
                          safeQueryOne findSql [ toSql $ editionName pubData ]
    where findSql = unlines [ "SELECT version"
                            , "FROM bookbrainz_v.edition_v"
                            , "WHERE name = ?"
                            ]

  newVersion pubData = fromSql `fmap`
                         queryOne insertSql [ toSql $ editionName pubData ]
    where insertSql = unlines [ "INSERT INTO bookbrainz_v.edition_v"
                              , "(name) VALUES (?)"
                              , "RETURNING version"
                              ]

--------------------------------------------------------------------------------
-- | Find all editions of a specific 'Book'.
-- The book must be a 'LoadedCoreEntity', ensuring it exists in the database.
findBookEditions :: HasDatabase m
                 => Ref (Tree Book)
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
