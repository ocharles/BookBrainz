module Main where

import           BookBrainz.Model.Book
import           BookBrainz.Model.Edition
import           BookBrainz.Model.Person
import           BookBrainz.Model.Publisher
import           BookBrainz.Script
import           BookBrainz.Search

reindexEverything :: Script ()
reindexEverything = do
  listAllBooks >>= mapM_ indexBook
  listAllEditions >>= mapM_ indexEdition
  allPersons >>= mapM_ indexPerson
  allPublishers >>= mapM_ indexPublisher

main :: IO ()
main = runScript reindexEverything
