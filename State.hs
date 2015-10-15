module Bidder.State where

import Control.Monad.State

type Stack = [Int]

push :: Int -> State Stack ()
push n =
  state $
  \stack -> ((),n : stack)

get' :: State Stack Stack
get' =
  state $
  (\s -> (s,s))

showState :: State Stack () -> Stack
showState st =
  snd $
  runState st []

work :: Stack -> IO ()
work previousState =
  do value <- getLine
     let n = read value
     let (_,newState) =
           runState (push n) previousState
     -- print the new state of the stack with the correct initial state, not an empty list.
     print $ newState
     work newState

main :: IO ()
main = work []
