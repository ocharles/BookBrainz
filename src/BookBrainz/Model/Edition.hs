-- | Functions for working with 'BookBrainz.Types.Edition.Edition' entities.
module BookBrainz.Model.Edition
       ( -- * Working With Editions
         findBookEditions
       ) where

import BrainzStem.Database (HasDatabase, query)
import BrainzStem.Model    (CoreEntity(..), HasTable(..), coreEntityFromRow
                           ,TableName(..), (!), rowKey)
import BookBrainz.Types

--------------------------------------------------------------------------------
instance HasTable Edition where
  tableName = TableName "edition"
  newFromRow row =
    Edition { editionName        = row ! "name"
            , editionFormat      = row ! "format"
            , editionBook        = row ! "book"
            , editionYear        = row ! "year"
            , editionPublisher   = row ! "publisher"
            , editionCountry     = row ! "country_iso_code"
            , editionLanguage    = row !  "language_iso_code"
            , editionIsbn        = row ! "isbn"
            , editionBarcode     = row ! "barcode"
            , editionIndex       = row ! "edition_index"
            }

instance CoreEntity Edition where

--------------------------------------------------------------------------------
-- | Find all editions of a specific 'Book'.
-- The book must be a 'LoadedCoreEntity', ensuring it exists in the database.
findBookEditions :: HasDatabase m
                 => LoadedCoreEntity Book         {-^ The book to find editions
                                                      of. -}
                 -> m [LoadedCoreEntity Edition]  {-^ A (possibly empty) list of
                                                      editions. -}
findBookEditions book = do
  results <- query selectQuery [ rowKey book ]
  return $ coreEntityFromRow `map` results
  where selectQuery = unlines [ "SELECT * "
                              , "FROM edition"
                              , "WHERE book = ?"
                              , "ORDER BY year, edition_index NULLS LAST"
                              ]
