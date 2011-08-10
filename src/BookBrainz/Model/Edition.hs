-- | Functions for working with 'BookBrainz.Types.Edition.Edition' entities.
module BookBrainz.Model.Edition
       ( -- * Working With Editions
         findBookEditions
       , getEdition
       ) where

import Data.Maybe

import Data.Map            (Map, (!))
import Data.UUID           (UUID)
import Database.HDBC       (SqlValue, toSql, fromSql)

import BookBrainz.Database (HasDatabase, query)
import BookBrainz.Types

--------------------------------------------------------------------------------
-- | Find all editions of a specific 'Book'.
-- The book must be a 'LoadedCoreEntity', ensuring it exists in the database
findBookEditions :: HasDatabase m
                 => LoadedCoreEntity Book         {-^ The book to find editions
                                                      of -}
                 -> m [LoadedCoreEntity Edition]  {-^ A (possibly empty) list of
                                                      editions -}
findBookEditions book = do
  results <- query selectQuery [ toSql $ rowKey book ]
  return $ editionFromRow `map` results
  where selectQuery = unlines [ "SELECT * "
                              , "FROM edition"
                              , "WHERE book = ?"
                              , "ORDER BY year, edition_index NULLS LAST"
                              ]

editionFromRow :: Map String SqlValue
               -> LoadedCoreEntity Edition
editionFromRow row =
  let edition = Edition { editionName        = fromSql $ row ! "name"
                        , editionFormat      = ref "format"
                        , editionBook        = fromSql $ row ! "book"
                        , editionYear        = fromSql $ row ! "year"
                        , editionPublisher   = ref "publisher"
                        , editionCountry     = ref "country"
                        , editionLanguage    = ref "language"
                        , editionIsbn        = fromSql $ row ! "isbn"
                        , editionBarcode     = fromSql $ row ! "barcode"
                        , editionIndex       = fromSql $ row ! "edition_index"
                        } in
  CoreEntity { gid               = fromSql $ row ! "gid"
             , coreEntityInfo    = edition
             , coreEntityVersion = fromSql $ row ! "version"
             }
    where ref column = Ref `fmap` fromSql (row ! column)

--------------------------------------------------------------------------------
-- | Get a single edition by GID.
getEdition :: HasDatabase m
           => UUID
           -- ^ The GID of the edition to load.
           -> m (Maybe (LoadedCoreEntity Edition))
           {-^ The loaded edition, or 'Nothing' if the edition could not be
           found. -}
getEdition bbid = do
  results <- query selectQuery [ toSql bbid ]
  return $ editionFromRow `fmap` listToMaybe results
  where selectQuery = unlines  [ "SELECT *"
                               , "FROM edition"
                               , "WHERE gid = ?" ]
