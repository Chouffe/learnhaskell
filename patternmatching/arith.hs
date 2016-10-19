module Arith where

roundTrip :: (Read b, Show a) => a -> b
roundTrip = read . show

main :: IO ()
main = print ((roundTrip 4) :: Int)
