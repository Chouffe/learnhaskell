module Main where

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

main :: IO ()
main = putStrLn "Hello World"

functionC :: Ord a => a -> a -> a
functionC x y = if x > y then x else y
-- functionC x y =
--   case x > y of
--     True -> x
--     False -> y

nums :: Num a => Integer -> a
nums x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10


oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = flip dodgy 2

mAbs :: Integer -> Integer
mAbs x
  | x < 0     = -x
  | otherwise = x

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  | otherwise = 'F'
  where y = x / 100

tensDigit :: Integral a => a -> a
tensDigit = snd . (`divMod` 10) . fst . (`divMod` 10)

-- foldBool :: a -> a -> Bool -> a
-- foldBool x y b =
--   case b of
--     True -> x
--     False -> y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
