module Main where

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Control.Monad.IO.Class (liftIO)
import           Network.AMQP

import           BookBrainz.Model.Book
import           BookBrainz.Model.Edition
import           BookBrainz.Model.Person
import           BookBrainz.Model.Publisher
import           BookBrainz.Script
import           BookBrainz.Types

main :: IO ()
main = runScript $ do
  chan <- liftIO $
    openConnection "127.0.0.1" "/" "guest" "guest" >>= openChannel

  mapM_ (dispatch chan) types

  where
    types = [ (bbids listAllBooks ,"book")
            , (bbids allEditions, "edition")
            , (bbids allPersons, "person")
            , (bbids allPublishers, "publisher")
            ]
    publish chan t bbid' = publishMsg chan "search" t
      newMsg { msgBody = LBS.pack bbid'
             , msgDeliveryMode = Just Persistent
             }
    dispatch chan (lister, queue) =
      lister >>= liftIO . mapM_ (publish chan queue)
    bbids = fmap (map (show . bbid))
