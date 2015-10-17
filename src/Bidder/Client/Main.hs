{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Char8 (pack)
import Control.Monad (forever)
import System.ZMQ4.Monadic
       (runZMQ, socket, Req(..), Sub(..), liftIO, connect, receive, send)

import Bidder.Utils

main :: IO ()
main =
  runZMQ $
  do print' "Connecting to server..."
     requester <- socket Req
     subscriber <- socket Sub
     connect requester "tcp://localhost:5555"
     print' "connected REQ at 5555"
     connect subscriber "tcp://localhost:5556"
     print' "connected SUB at 5556"
     welcomeStr <- receive subscriber
     print' welcomeStr
     forever $ do
        bidStr <- liftIO getLine
        let bid = read bidStr :: Int
        print' $ "Bidding at " ++ show bid
        send requester [] $ pack $ show bid
        ack <- receive requester
        print' $ "Response: " ++ show ack
        auctionStatus <- receive subscriber
        print' "Auction status"
        print' auctionStatus
