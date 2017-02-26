module Main where

import Lib (check, State(..), StateTransition, AcceptingStates)

table :: StateTransition
table A A = [A, C]
table A B = [D]
table A C = [B]
table A D = [D]
table B A = [B]
table B B = [A, B]
table B C = [A]
table B D = [D]
table C A = [D]
table C B = [A]
table C C = [B]
table C D = [D]
table D A = [C]
table D B = [A]
table D C = [A]
table D D = [D]

acceptingStates :: AcceptingStates
acceptingStates = [C, D]

main :: IO ()
main = mapM_ (putStrLn . show . (check table acceptingStates))
  [ [A, A, B, B]
  , [A, D, A]
  , [C, D, B]
  , [A, A, A]
  , [ B, B, B, B, B, B]
  ]
