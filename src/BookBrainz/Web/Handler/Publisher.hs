{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Handlers for the @/publisher@ resource.
module BookBrainz.Web.Handler.Publisher
       ( showPublisher
       , addPublisher
       ) where

import           Control.Applicative        ((<$>))
import           Data.Traversable           (traverse)

import           Data.ByteString.Char8      (pack)
import           Data.Copointed             (copoint)
import           Snap.Core
import           Text.Digestive.Blaze.Html5 (renderFormHtml)
import           Text.Digestive.Forms.Snap  (eitherSnapForm)

import qualified BookBrainz.Forms as Forms
import           BookBrainz.Types
import           BookBrainz.Model.Publisher (publishedEditions)
import           BookBrainz.Web.Handler  (output, onNothing, withUser)
import           BookBrainz.Web.Snaplet  (BookBrainzHandler)
import qualified BookBrainz.Web.View.Publisher as V
import           BrainzStem.Database (withTransaction)
import           BrainzStem.Model

--------------------------------------------------------------------------------
{-| Show a single 'Publisher', searching by it's BBID. If the publisher cannot
be found, a 404 page is displayed. -}
showPublisher :: BBID Publisher -> BookBrainzHandler ()
showPublisher bbid' = do
  publisher <- getByBbid bbid' `onNothing` "Publisher not found"
  editions <- publishedEditions (coreEntityConcept publisher) >>= mapM loadEdition
  output $ V.showPublisher publisher editions
  where loadEdition e =
          (e, ) <$> traverse getByConcept (editionPublisher . copoint $ e)

--------------------------------------------------------------------------------
-- | Display a form for adding new publishers, add on submission add the
-- publisher.
addPublisher :: BookBrainzHandler ()
addPublisher = do
  withUser $  \user -> do
    r <- eitherSnapForm Forms.addPublisher "book"
    case r of
      Left form' -> output $ V.addPublisher $ renderFormHtml form'
      Right submission -> do
        publisher <- withTransaction  $ create submission $ entityRef user
        redirect $ pack . ("/publisher/" ++) . show . bbid $ publisher
