{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module BookBrainz.Forms where

import           Control.Applicative         ((<$>))
import           Data.Maybe                  (fromMaybe)

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Snap.Core
import           Text.Blaze.Html5            (Html, (!), toValue)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive
import           Text.Digestive.Blaze.Html5  hiding (inputText)
import           Text.Digestive.Forms        (FormInput(..))
import qualified Text.Digestive.Forms        as Forms
import           Text.Digestive.Forms.Snap

import           BookBrainz.Types.Book

data SearchQuery = SearchQuery { query :: Text }

bookForm :: (Monad m, MonadSnap m)
         => Maybe Book
         -> SnapForm m Html BlazeFormHtml Book
bookForm book = Book <$> inputText (bookName `fmap` book)

nonEmpty :: Monad m => Validator m Html Text
nonEmpty = check "Query cannot be empty" (not . T.null)

searchForm :: (Monad m, MonadSnap m) => SnapForm m Html BlazeFormHtml SearchQuery
searchForm = SearchQuery <$> inputText (Just "") `validate` nonEmpty

inputText :: (Monad m, Functor m, FormInput i f)
          => Formlet m i e BlazeFormHtml Text
inputText = Forms.inputText $ \id' inp -> createFormHtml $ \_ ->
  H.input ! A.type_ "text"
          ! A.name (toValue $ show id')
          ! A.value (toValue $ fromMaybe "" inp)

processForm :: (MonadSnap m)
            => SnapForm m e v a  -- ^ Form
            -> String            -- ^ Form name
            -> m (Either v a)    -- ^ Result
processForm form name = eitherForm form name snapEnvironment
