module Main where

mcurry :: ((a, b) -> c) -> a -> b -> c
mcurry f x y = f (x, y)

muncurry :: (a -> b -> c) -> (a, b) -> c
muncurry = _

main :: IO ()
main = putStrLn "Hello!"
