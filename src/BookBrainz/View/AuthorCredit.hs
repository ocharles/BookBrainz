module BookBrainz.View.AuthorCredit
       ( linkAuthorCredit
       ) where

import BookBrainz.Types
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, Html, (!), toValue)
import Text.Blaze.Html5.Attributes
import Data.UUID (toString)

linkAuthorCredit :: AuthorCredit -> Html
linkAuthorCredit authorCredit =
  formatCredit `mapM_` authorCredits authorCredit
    where formatCredit credit = do
            H.a ! href (uri credit) $ toHtml $ creditedName credit
            toHtml $ creditedJoinPhrase credit
          uri credit = toValue $ "/person/" ++ toString (personGid $ creditedAuthor credit)

