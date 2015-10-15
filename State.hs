module Bidder.State where

import Control.Monad.State
import Debug.Trace (traceShow)

type Stack = [Int]

traceShow' :: (Show b)
           => b -> b
traceShow' arg = traceShow arg arg

push :: Int -> State Stack ()
push n =
  state $
  \stack -> ((),n : stack)

get' :: State Stack Stack
get' = state $ (\s -> (s,s))

showState :: State Stack () -> Stack
showState st = snd $ runState st []

work :: Int -> State Stack () -> State Stack ()
work value = do return $ push value

ioWork :: State Stack () -> IO ()
ioWork previousState =
  do value <- getLine
     let n = read value
     let newState = work n previousState
     ioWork newState

main :: IO ()
main =
  do ioWork $
       state $
       (\_ -> ((),[]))
