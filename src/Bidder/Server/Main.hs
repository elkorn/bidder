{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.State (runState)
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (listToMaybe)
import System.ZMQ4 (version)
import System.ZMQ4.Monadic
       (runZMQ, socket, Socket, Router(..), Pub(..), ZMQ, bind, receive,
        send)

import Bidder.State
import Bidder.Utils

initialState :: Stack
initialState = []

type AuctionFunction z x = Stack -> Socket z Pub -> Socket z Router -> ZMQ z x

work :: AuctionFunction z ()
work previousState publisherSocket receiverSocket =
  do buffer <- receive receiverSocket
     print' buffer
     case (unpack buffer) of
       "JOIN" ->
         do waitForJoin 1 previousState publisherSocket receiverSocket
       str ->
         do newState <-
              receiveBid str previousState publisherSocket receiverSocket
            work newState publisherSocket receiverSocket

receiveBid :: String -> AuctionFunction z Stack
receiveBid bidStr previousState publisherSocket receiverSocket =
  do print' $ "Got bid: " ++ bidStr
     send receiverSocket [] "ACK"
     let n = read bidStr :: Int
     let topBid =
           maybe 0 id $
           listToMaybe previousState
     let (_,newState) =
           if n > topBid
              then runState (push n) previousState
              else ((),previousState)
     print' newState
     print' "Publishing new state to clients..."
     send publisherSocket [] $
       pack $
       unwords ["10001",show newState]
     return newState

-- TODO: (() -> ZMQ z ()) is guerilla laziness. Test if Haskell/zeromq impl. does that for us.
receiveJoin :: Socket z Router -> (() -> ZMQ z ()) -> ZMQ z ()
receiveJoin receiverSocket continuation =
  do print' "Waiting for a join..."
     buffer <- receive receiverSocket
     print' $
       unwords ["Got",show buffer,"in receiveJoin"]
     case (unpack buffer) of
       "JOIN" -> continuation ()
       _ ->
         do send receiverSocket [] $
              ""
            receiveJoin receiverSocket continuation

waitForJoin :: Int -> AuctionFunction z ()
waitForJoin 0 previousState publisherSocket receiverSocket =
  work previousState publisherSocket receiverSocket
waitForJoin n previousState publisherSocket receiverSocket =
  do send receiverSocket [] $
       pack $ show previousState
     send publisherSocket [] $
       pack $
       unwords ["10001","Clients joining, please wait..."]
     if (n > 1)
        then receiveJoin receiverSocket continue  -- drop bids until the client joins
        else continue ()
  where continue =
          (\() ->
             waitForJoin (n - 1)
                         previousState
                         publisherSocket
                         receiverSocket)
main :: IO ()
main =
  do (major,minor,patch) <- version
     putStrLn $
       unwords ["Server running 0MQ",show major,show minor,show patch]
     runZMQ $
       do publisher <- socket Pub
          receiver <- socket Router
          bind receiver "tcp://*:5555"
          bind publisher "tcp://*:5556"
          receiveJoin
            receiver
            (\() ->
               waitForJoin 1 initialState publisher receiver)
