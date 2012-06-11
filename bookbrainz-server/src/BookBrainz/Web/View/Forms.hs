{-# LANGUAGE OverloadedStrings #-}
module BookBrainz.Web.View.Forms where

import           Data.Text (Text)
import           Text.Blaze.Html5    (Html, (!), toValue)
import           Text.Digestive.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive.View as Form

type Renderer = Text -> Form.View Html -> Html

fieldRow :: Form.View Html -> Text -> Html -> Renderer -> Html
fieldRow v name label' view =
  H.p $ do
    label name v label'
    view name v
    errorList name v

fieldTable :: Form.View Html -> [(Text, Html, Renderer)] -> Html
fieldTable v = mapM_ (row v)
  where row v' (name, label', r) = fieldRow v' name label' r

submitRow :: Text -> Html
submitRow submitLabel =
  H.p ! A.class_ "indent" $
    H.input ! A.type_ "submit" ! A.value (toValue submitLabel)

checkboxRow :: Form.View Html -> Text -> Html -> Html
checkboxRow v name label' = do
  H.p ! A.class_ "indent" $ do
    inputCheckbox name v
    label name v label'
