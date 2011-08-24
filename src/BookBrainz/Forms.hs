{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module BookBrainz.Forms where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Digestive
import Text.Digestive.Forms.Html
import qualified Text.Digestive.Forms as Forms
import Text.Blaze.Html5 (Html, (!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Digestive.Forms.Snap
import Text.Digestive.Forms (FormInput(..))
import Text.Digestive.Blaze.Html5 hiding (inputText)
import Snap.Types

import BookBrainz.Types.Book

data SearchQuery = SearchQuery { query :: Text }

bookForm :: (Monad m, MonadSnap m) => SnapForm m Html BlazeFormHtml Book
bookForm = Book <$> inputText (Just "")

searchForm :: (Monad m, MonadSnap m) => SnapForm m Html BlazeFormHtml SearchQuery
searchForm = SearchQuery <$> inputText (Just "")

inputText :: (Monad m, Functor m, FormInput i f)
          => Formlet m i e BlazeFormHtml Text
inputText = Forms.inputText $ \id' inp -> createFormHtml $ \_ ->
  H.input ! A.type_ "text"
          ! A.name (toValue $ show id')
          ! A.value (toValue $ fromMaybe "" inp)
