-- | Functions for working with 'BookBrainz.Types.Edition.Edition' entities.
module BookBrainz.Model.Edition
       ( -- * Working With Editions
         findBookEditions
       ) where

import Data.Map            ((!))
import Database.HDBC       (toSql, fromSql)

import BookBrainz.Database (HasDatabase, query)
import BookBrainz.Model    (CoreEntity(..), coreEntityFromRow, TableName(..))
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
  return $ coreEntityFromRow `map` results
  where selectQuery = unlines [ "SELECT * "
                              , "FROM edition"
                              , "WHERE book = ?"
                              , "ORDER BY year, edition_index NULLS LAST"
                              ]

--------------------------------------------------------------------------------
instance CoreEntity Edition where
  tableName = TableName "edition"
  newFromRow row =
    Edition { editionName        = fromSql $ row ! "name"
            , editionFormat      = ref "format"
            , editionBook        = fromSql $ row ! "book"
            , editionYear        = fromSql $ row ! "year"
            , editionPublisher   = ref "publisher"
            , editionCountry     = ref "country"
            , editionLanguage    = ref "language"
            , editionIsbn        = fromSql $ row ! "isbn"
            , editionBarcode     = fromSql $ row ! "barcode"
            , editionIndex       = fromSql $ row ! "edition_index"
            }
      where ref column = Ref `fmap` fromSql (row ! column)
