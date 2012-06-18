{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromJust)
import Data.Traversable (traverse)
import Network.AMQP

import BrainzStem.Model (getByBbid)
import BookBrainz.Script
import BookBrainz.Search (indexBook)
import BookBrainz.Types

handleIndex :: (Message, Envelope) -> Script ()
handleIndex (msg, env) = do
  getByBbid bbid' >>= traverse indexBook
  liftIO $ do
    putStrLn $ "Indexed " ++ show bbid'
    ackEnv env
  where bbid' = (fromJust . parseBbid . LBS.unpack . msgBody $ msg)

main :: IO ()
main = runScript $ void $ liftIO $ do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  declareQueue chan newQueue {queueName = "database"}
  declareExchange chan newExchange
      { exchangeName = "database"
      , exchangeType = "fanout"
      }
  bindQueue chan "" "database" "database"

  consumeMsgs chan "database" Ack (runScript . handleIndex)
  forever getLine
