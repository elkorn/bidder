module Bidder.State where

import Control.Monad.Trans.State (State, state, runState)

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
