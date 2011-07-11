module BookBrainz.View.AuthorCredit
       ( linkAuthorCredit
       ) where

import BookBrainz.Types
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, Html)

linkAuthorCredit :: AuthorCredit -> Html
linkAuthorCredit authorCredit =
  toHtml $ T.concat $ formatCredit `map` (authorCredits authorCredit)
    where formatCredit credit = T.concat [ (creditedName credit), (creditedJoinPhrase credit) ]
