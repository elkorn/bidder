module Bidder.Utils where

import System.ZMQ4.Monadic (ZMQ, liftIO)

print' :: (Show a)
       => a -> ZMQ z ()
print' = liftIO . print . show
