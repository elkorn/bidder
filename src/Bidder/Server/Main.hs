{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.State (runState)
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (listToMaybe)
import System.ZMQ4 (version)
import System.ZMQ4.Monadic
       (runZMQ, socket, Socket, Rep(..), Pub(..), ZMQ, bind, receive,
        send)

import Bidder.State
import Bidder.Utils

initialState :: Stack
initialState = []

work :: Stack -> Socket z Pub -> Socket z Rep -> ZMQ z ()
work previousState publisherSocket receiverSocket =
  do buffer <- receive receiverSocket
     let str = unpack buffer
     print' $ "Got bid: " ++ str
     send receiverSocket [] "ACK"
     let n = read str :: Int
     let topBid = maybe 0 id $ listToMaybe previousState
     let (_,newState) =
           if n > topBid
              then runState (push n) previousState
              else ((),previousState)
     print' newState
     send publisherSocket [] $
       pack $ show newState
     work newState publisherSocket receiverSocket

main :: IO ()
main =
  do (major,minor,patch) <- version
     putStrLn $
       unwords ["Server running 0MQ",show major,show minor,show patch]
     runZMQ $
       do publisher <- socket Pub
          receiver <- socket Rep
          bind receiver "tcp://*:5555"
          bind publisher "tcp://*:5556"
          send publisher [] $ pack "Starting the auction!"
          work initialState publisher receiver--   do buffer <- receive responder
                                              --      liftIO $
                                              --        do putStrLn $ "received " ++ show buffer
                                              --           threadDelay 1000
                                              --      send responder [] "World"
