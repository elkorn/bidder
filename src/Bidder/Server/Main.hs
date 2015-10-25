{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.State (runState)
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (listToMaybe)
import qualified Data.List.NonEmpty as NE
import System.ZMQ4 (version)
import System.ZMQ4.Monadic
       (runZMQ, socket, Socket, Router(..), Pub(..), ZMQ, bind, receive,
        send, sendMulti)

import Bidder.State
import Bidder.Utils

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
            (\message ->
               waitForJoin message 1 initialState publisher receiver)

initialState :: Stack
initialState = []

type AuctionFunction z x = Stack -> Socket z Pub -> Socket z Router -> ZMQ z x

data Message =
  Message String
          String
  deriving (Show,Eq)

sendMessage :: Socket z Router -> Message -> ZMQ z ()
sendMessage receiverSocket (Message ident message) =
  do sendMulti receiverSocket $
       NE.fromList $
       map pack [ident,"",message]

receiveMessage :: Socket z Router -> ZMQ z Message
receiveMessage receiverSocket =
  do identityBuffer <- receive receiverSocket
     _ <- receive receiverSocket
     messageBuffer <- receive receiverSocket
     return $
       Message (unpack identityBuffer)
               (unpack messageBuffer)

work :: AuctionFunction z ()
work previousState publisherSocket receiverSocket =
  do msg@(Message ident message) <- receiveMessage receiverSocket
     print' $ ident ++ ": " ++ message
     case message of
       "JOIN" ->
         do waitForJoin msg 1 previousState publisherSocket receiverSocket
       _ ->
         do newState <-
              receiveBid msg previousState publisherSocket receiverSocket
            work newState publisherSocket receiverSocket

waitForJoin :: Message -> Int -> AuctionFunction z ()
waitForJoin _ 0 previousState publisherSocket receiverSocket =
  work previousState publisherSocket receiverSocket
waitForJoin msg@(Message ident _) n previousState publisherSocket receiverSocket =
  do sendMessage receiverSocket
                 (Message ident (show previousState))
     send publisherSocket [] $
       pack $
       unwords ["10001","Client", ident, "has joined the auction."]
     if (n > 1)
        then receiveJoin receiverSocket
                         (\_ -> continue ())  -- drop bids until the client joins
        else continue ()
  where continue =
          (\() ->
             waitForJoin msg
                         (n - 1)
                         previousState
                         publisherSocket
                         receiverSocket)

receiveJoin :: Socket z Router -> (Message -> ZMQ z x) -> ZMQ z x
receiveJoin receiverSocket continuation =
  do print' "Waiting for a join..."
     msg@(Message ident message) <- receiveMessage receiverSocket
     print' $ ident ++ ": " ++ message ++ " (in receiveJoin)"
     case message of
       "JOIN" -> continuation msg
       _ ->
         do send receiverSocket [] $
              "Try again!"
            receiveJoin receiverSocket continuation
receiveBid :: Message -> AuctionFunction z Stack
receiveBid (Message ident bidStr) previousState publisherSocket receiverSocket =
  do print' $ "Got bid: " ++ bidStr
     sendMessage receiverSocket (Message ident "ACK")
     let n = read bidStr :: Int
     let topBid =
           maybe 0 id $
           listToMaybe previousState
     let (_,newState) =
           if n > topBid
              then runState (push n) previousState
              else ((),previousState)
     print' newState
     send publisherSocket [] $
       pack $
       unwords ["10001",show newState]
     return newState
