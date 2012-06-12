{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever, join, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe, fromJust)
import Data.Traversable (traverse)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.Notification (getNotification, notificationChannel)
import Snap.Snaplet.PostgresqlSimple (query, withPG, execute_)

import BrainzStem.Model (getByBbid)
import BookBrainz.Script
import BookBrainz.Search (indexBook)
import BookBrainz.Types

data Event = Event { eventId :: Int, eventBbid :: BBID Book }

instance FromRow Event where
  fromRow = Event <$> field <*> fmap (fromJust . parseBbid) field

dispatchEvent :: Event -> Script ()
dispatchEvent ev = do
  getByBbid (eventBbid ev) >>= traverse indexBook
  liftIO $ putStrLn $ "Indexed " ++ show (eventBbid ev)

runBatch :: Int -> Script ()
runBatch batchId = do
  query "SELECT ev_id, ev_data FROM pgq.get_batch_events(?)" (Only batchId) >>=
    mapM_ dispatchEvent
  void $ (query "SELECT pgq.finish_batch(?)" (Only batchId) :: Script [Only Int])

consumeBatches :: Script ()
consumeBatches = do
  batchId <- nextBatchId
  case batchId of
    Just b -> runBatch b >> consumeBatches
    Nothing -> return ()

indexer :: Script ()
indexer = do
  liftIO $ putStrLn "Consuming existing batches"
  consumeBatches

  liftIO $ putStrLn "Waiting for index events"
  execute_ "LISTEN pgqticker"
  forever $ do
    n <- withPG $ \c -> getNotification c
    when (notificationChannel n == "pgqticker") consumeBatches

nextBatchId :: Script (Maybe Int)
nextBatchId =
  (join . fmap fromOnly . listToMaybe) <$> query "SELECT pgq.next_batch(?, ?)"
    ("foo" :: String, "bar" :: String)

main :: IO ()
main = runScript indexer
