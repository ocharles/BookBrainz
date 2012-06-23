{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<$>))
import           Control.Monad (forever, void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe (fromJust)
import           GHC.IO.Handle.FD (stderr)
import           Network.AMQP
import           System.Log.Formatter (simpleLogFormatter)
import           System.Log.Handler (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger

import           BookBrainz.Model.Publisher ()
import           BookBrainz.Script
import           BookBrainz.Search (indexBook, indexEdition, indexPerson, indexPublisher)
import           BookBrainz.Types
import           BrainzStem.Model (CoreEntity)
import           BrainzStem.Model (getByBbid)

handleIndex' :: CoreEntity a => (Message, Envelope) -> String
             -> (LoadedCoreEntity a -> Script ()) -> Script ()
handleIndex' (msg, env) logClass indexer = do
  ent <- getByBbid bbid'
  case ent of
    Nothing -> liftIO $ warningM logClass $
      "Could not index " ++ show bbid' ++ " - entity not found"
    Just e -> do
      indexer e
      liftIO $ noticeM logClass $ "Indexed " ++ show bbid'
  liftIO $ ackEnv env
  where bbid' = (fromJust . parseBbid . LBS.unpack . msgBody $ msg)

handleIndexBook :: (Message, Envelope) -> Script ()
handleIndexBook m =
  handleIndex' m "BookBrainz.Search.Book" indexBook

handleIndexEdition :: (Message, Envelope) -> Script ()
handleIndexEdition m =
  handleIndex' m "BookBrainz.Search.Edition" indexEdition

handleIndexPublisher :: (Message, Envelope) -> Script ()
handleIndexPublisher m =
  handleIndex' m "BookBrainz.Search.Publisher" indexPublisher

handleIndexPerson :: (Message, Envelope) -> Script ()
handleIndexPerson m =
  handleIndex' m "BookBrainz.Search.Person" indexPerson

main :: IO ()
main = runScript $ void $ liftIO $ do
  lh <- formatLogging <$> streamHandler stderr DEBUG
  updateGlobalLogger rootLoggerName (setLevel NOTICE)
  updateGlobalLogger rootLoggerName (setHandlers [lh])

  noticeM "BookBrainz.Search" "Connecting to RabbitMQ"
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  noticeM "BookBrainz.Search" "Declaring 'search' exchange"
  declareExchange chan newExchange
      { exchangeName = "search"
      , exchangeType = "direct"
      }

  mapM (\(n, indexer) ->
      do noticeM "BookBrainz.Search" $ "Establishing queue: " ++ n
         declareQueue chan newQueue {queueName = n}
         bindQueue chan n "search" n
         consumeMsgs chan n Ack (runScript . indexer))
    [ ("book", handleIndexBook)
    , ("edition", handleIndexEdition)
    , ("person", handleIndexPerson)
    , ("publisher", handleIndexPublisher)
    ]

  noticeM "BookBrainz.Search" "All queues setup, awaiting events"
  forever getLine
  where formatLogging lh = setFormatter lh $
          simpleLogFormatter "[$time : $loggername] $prio $msg"
