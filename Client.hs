{-# LANGUAGE OverloadedStrings #-}
module Bidder.Client where

import Control.Monad (forM_)
import System.ZMQ4.Monadic
       (runZMQ, socket, Req(..), liftIO, connect, receive, send)

main :: IO ()
main =
  runZMQ $
  do
    liftIO $ putStrLn "Connecting to server..."
    requester <- socket Req
    connect requester "tcp://localhost:5555"

    forM_ [1..10] $ \i -> do
      liftIO . putStrLn $ unwords ["Sending Hello", show i, "..."]
      send requester [] $ "Hello"
      buffer <- receive requester
      liftIO . putStrLn $ "Received " ++ (show buffer)
  
