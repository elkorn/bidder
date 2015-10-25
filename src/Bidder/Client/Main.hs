{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Char8 (pack, unpack, ByteString)
import Control.Monad (forever)
import System.ZMQ4.Monadic
       (runZMQ, socket, Req(..), Sub(..), liftIO, connect, receive, send,
        subscribe)

import Bidder.Utils

getAuctionState :: ByteString -> String
getAuctionState bs = unwords $ tail $ words $ unpack bs

main :: IO ()
main =
  runZMQ $
  do print' "Connecting to server..."
     subscriber <- socket Sub
     connect subscriber "tcp://localhost:5556"
     subscribe subscriber (pack "10001")
     print' "connected SUB at 5556"
     requester <- socket Req
     connect requester "tcp://localhost:5555"
     print' "connected REQ at 5555"
     send requester [] $
       pack "JOIN"
     print' "Sent JOIN request..."
     initialState <- receive requester
     print' $ "Current auction state: " ++ show initialState
     forever $
       do auctionStatus <- receive subscriber
          print' $
            unwords ["[PUB]"
                    ,"Current auction state:"
                    ,getAuctionState auctionStatus]
          print' "Your bid: "
          bidStr <- liftIO getLine
          let bid = read bidStr :: Int
          print' $ "Bidding at " ++ show bid
          send requester [] $
            pack $ show bid
          ack <- receive requester
          print' $ "Response: " ++ show ack
