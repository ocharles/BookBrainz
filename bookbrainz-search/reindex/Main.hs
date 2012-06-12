module Main where

import           BookBrainz.Model.Book
import           BookBrainz.Script
import           BookBrainz.Search
import           BookBrainz.Types
import           BrainzStem.Model

reindexEverything :: Script ()
reindexEverything = listAllBooks >>= mapM_ indexBook

main = runScript reindexEverything
