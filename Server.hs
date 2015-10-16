{-# LANGUAGE OverloadedStrings #-}
module Bidder.Server where

import Control.Concurrent 
import Control.Monad (forever)
import Control.Monad.State.Lazy (State(..), get, put)
import System.ZMQ4 (version)
import System.ZMQ4.Monadic
       (runZMQ, socket, Rep(..), Pub(..), liftIO, bind, receive, send)
import Data.ByteString.Char8 (pack)

main :: IO ()
main =
  do (major,minor,patch) <- version
     putStrLn $
       unwords ["Server running 0MQ",show major,show minor,show patch]
     runZMQ $
       do publisher <- socket Pub
          responder <- socket Rep
          bind responder "tcp://*:5555"
          bind publisher "tcp://*:5556"
          forever $
            do buffer <- receive responder
               liftIO $
                 do putStrLn $ "received " ++ show buffer
                    threadDelay 1000
               send responder [] "World"
