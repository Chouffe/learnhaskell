module Main where

import Data.List (sort)

main :: IO ()
main = putStrLn "Hello World"

data TisAnInteger =
  TisAn Integer

data TwoIntegers =
  Two Integer Integer

data StringOrInt =
    TisAnInt Int
  | TisAString String

data Pair a =
  Pair a a

data Tuple a b =
  Tuple a b

data Which a =
    ThisOne a
  | ThatOne a

data EitherOr a b =
    Hello a
  | Goodbye b


instance Eq TisAnInteger where
  TisAn x == TisAn y = x == y

instance Eq TwoIntegers where
  Two a b == Two c d = a == c && b == d

instance Eq StringOrInt where
  TisAnInt x == TisAnInt y = x == y
  TisAString x == TisAString y = x == y
  _ == _ = False

instance Eq a => Eq (Pair a) where
  Pair x y == Pair a b = x == a && y == b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple x y == Tuple u v = x == u && y == v

instance Eq a => Eq (Which a) where
  ThisOne x == ThisOne y = x == y
  ThatOne x == ThatOne y = x == y
  _ == _ = False

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello x == Hello y = x == y
  Goodbye x == Goodbye y = x == y
  _ == _ = False

-- Exercises

data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson = print

data Mood =
    Blah
  | Woot
  deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x =
  if x == Woot
     then Blah
     else x

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah deriving (Eq, Show)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

i :: Num a => a
i = 1

-- f :: Float
fii :: Fractional a => a
fii = 1.0

freud :: Int -> Int
freud x = x

myX :: Int
myX = 1

sigmund :: Int -> Int
sigmund _ = myX

-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung = minimum

mySort :: String -> String
mySort = sort

signifier :: String -> Char
signifier = head . mySort

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f _ = f
