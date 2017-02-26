module Lib
  ( check
  , AcceptingStates
  , State(..)
  , StateTransition
  )
  where

import Data.List (nub)

data State = A | B | C | D deriving (Eq, Ord, Show)
type AcceptingStates = [State]
type StateTransition = State -> State -> [State]

nextLayer :: StateTransition -> [State] -> [[State]]
nextLayer _ []         = []
nextLayer table states = nub $ foldr (\a b -> [x : ys | x <- a, ys <- b]) [[]] leftRightinputs
  where leftRightinputs = zipWith table states (tail states)

check :: StateTransition -> AcceptingStates -> [State] -> Bool
check _ [] _                       = False
check _ _ []                       = False
check _ acceptingStates [state]    = state `elem` acceptingStates
check table acceptingStates states = any (check table acceptingStates) rootStates
  where n = length states
        rootStates = iterate (>>= nextLayer table) (return states) !! (n - 1)
