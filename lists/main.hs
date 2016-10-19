module Main where

firstSen :: String
firstSen = "Tyger Tyger, burning bright\n"

secondSen :: String
secondSen = "In the forests of the night\n"

thirdSen :: String
thirdSen = "What immortal hand or eye\n"

fourthSen :: String
fourthSen = "Could frame thy fearful symmetry?"

sentences :: String
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

main :: IO ()
main = putStrLn "Hello"

myWords :: String -> [String]
myWords = aux ' '

myLines :: String -> [String]
myLines = aux '\n'

aux :: Char ->String -> [String]
aux _ "" = []
aux separator s@(x : xs)
  | separator == x = aux separator xs
  | s == t         = [s]
  | otherwise     = t : aux separator (dropWhile (/= separator) s)
  where t = takeWhile (/= separator) s

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Integer]
myCube = [x^3 | x <- [1..5]]

myTup :: [(Integer, Integer)]
myTup = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

n :: Int
n = length myTup

mzip :: [t] -> [t1] -> [(t, t1)]
mzip xs [] = []
mzip [] ys = []
mzip (x:xs) (y:ys) = (x, y) : mzip xs ys

mzipWith :: (t2 -> t -> t1) -> [t2] -> [t] -> [t1]
mzipWith _ xs [] = []
mzipWith _ [] ys = []
mzipWith f (x : xs) (y : ys) = f x y : mzipWith f xs ys

mzipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
mzipWith2 f xs ys = map (uncurry f) $ zip xs ys
