import           BrainzStem.Model
import           BookBrainz.Model.Book
import           BookBrainz.Model.Role
import           BookBrainz.Script
import qualified BookBrainz.Search     as Search

reindexEverything :: Script ()
reindexEverything = listAllBooks >>= mapM_ indexBook
   where indexBook b = findRoles b >>= Search.indexBook b

main = runScript reindexEverything

