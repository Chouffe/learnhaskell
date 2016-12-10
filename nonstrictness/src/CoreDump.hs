module CoreDump where

discriminatory :: Bool -> Int
discriminatory b =
  let x = undefined
   in case seq x b of
    False -> 0
    True -> 1
