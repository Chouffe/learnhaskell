module Lib where

import qualified Data.Map as Map


data State = A | B | C | D deriving (Eq, Ord, Show)
type AcceptingStates = [State]

table :: State -> State -> [State]
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

check :: AcceptingStates -> [State] ->Bool
check [] _ = False  -- No accepting states
check _ [] = False  -- No state
check acceptingStates [state] = state `elem` acceptingStates  -- Is state in accepting states?
-- Recursively generate states
check acceptingStates states@(_:xs) = any (check acceptingStates) $ foldr reducer [[]] $ zipWith table states xs
  where reducer zs yss = [x : ys | x <- zs, ys <- yss]

output :: IO ()
output = do
  mapM_ (putStrLn . show . (check [C, D])) [ [A, A, B, B]
                                           , [A, D, A]
                                           , [C, D, B]
                                           , [A, A, A]
                                           , [ B, B, B, B, B, B]
                                           ]
