module Lib where

newtype Identity a = Identity a

data Pair a = Pair a a

data Two a b = Two a b

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data TalkToMe a =
    Halt
  | Print a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x) = Print $ f x
  fmap f (Read g) = Read $ \s -> f (g s)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
