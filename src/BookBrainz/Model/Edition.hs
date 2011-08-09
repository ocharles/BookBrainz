-- | Functions for working with 'BookBrainz.Types.Edition.Edition' entities.
module BookBrainz.Model.Edition
       ( findBookEditions
       ) where

import Data.Map            ((!))
import Database.HDBC       (toSql, fromSql)

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
  return $ fromRow `map` results
  where selectQuery = unlines [ "SELECT * "
                              , "FROM edition"
                              , "WHERE book = ?"
                              , "ORDER BY year, edition_index NULLS LAST"
                              ]
        fromRow row = CoreEntity { gid               = fromSql $ row ! "gid"
                                 , coreEntityInfo    = editionFromRow row
                                 , coreEntityVersion = fromSql $ row ! "version"
                                 }
        editionFromRow row = Edition { editionName        = fromSql $ row ! "name"
                                     , editionFormat      = maybeReference row "format"
                                     , editionBook        = toRef book
                                     , editionYear        = fromSql $ row ! "year"
                                     , editionPublisher   = maybeReference row "publisher"
                                     , editionCountry     = maybeReference row "country"
                                     , editionLanguage    = maybeReference row "language"
                                     , editionIsbn        = fromSql $ row ! "isbn"
                                     , editionBarcode     = fromSql $ row ! "barcode"
                                     , editionIndex       = fromSql $ row ! "edition_index"
                                     }
        maybeReference row column = Ref `fmap` fromSql (row ! column)
