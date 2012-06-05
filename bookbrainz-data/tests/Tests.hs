import Test.BrainzStem
import Test.Framework.Runners.Console (defaultMain)

import qualified Test.BookBrainz.Book as Book
import qualified Test.BookBrainz.Country as Country
import qualified Test.BookBrainz.EditionFormat as EditionFormat
import qualified Test.BookBrainz.Edition as Edition
import qualified Test.BookBrainz.Language as Language
import qualified Test.BookBrainz.Person as Person
import qualified Test.BookBrainz.Publisher as Publisher
import qualified Test.BookBrainz.Role as Role

main :: IO ()
main = defaultMain [ testGroup "Model.Book" Book.tests
                   , testGroup "Model.Country" Country.tests
                   , testGroup "Model.EditionFormat" EditionFormat.tests
                   , testGroup "Model.Edition" Edition.tests
                   , testGroup "Model.Language" Language.tests
                   , testGroup "Model.Person" Person.tests
                   , testGroup "Model.Publisher" Publisher.tests
                   , testGroup "Model.Role" Role.tests
                   ]
