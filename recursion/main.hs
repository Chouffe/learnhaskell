module Main where

import Data.List (intercalate)
import Data.Function (fix)

msum :: (Eq a, Num a) => a -> a
msum 0 = 0
msum n = n + msum (n - 1)

mtime :: Integral a => a -> a -> a
mtime 1 x = x
mtime x 1 = x
mtime x y = y + mtime (x - 1) y

main :: IO ()
main = putStrLn "Hello"

mc91 :: (Num a, Ord a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits d ++ [m]
  where (d, m) = divMod n 10

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

factorial :: Int -> Int
factorial = fix (\f -> \n -> if n <= 1 then n else n * f (n - 1))

fib :: Int -> Int
fib = fix (\f -> \n -> if n < 2 then n else f (n - 1) + f (n - 2))

digits' :: Int -> [Int]
digits' =
  fix $ \f n ->
        if n < 10 then [n]
        else
          let (d, m) = divMod n 10
          in (f d) ++ [m]

-- >>> take 10 (fix (1:)) :: [Int]
