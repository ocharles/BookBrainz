module Main where

import           BookBrainz.Model.Book
import           BookBrainz.Model.Role
import           BookBrainz.Script
import qualified BookBrainz.Search.Indexer as Search
import           BookBrainz.Types
import           BrainzStem.Model

reindexEverything :: Script ()
reindexEverything = listAllBooks >>= mapM_ indexBook

indexBook :: LoadedCoreEntity Book -> Script ()
indexBook b = findRoles (coreEntityTree b) >>= Search.indexBook b

main = runScript reindexEverything
