module Lib where

import Test.QuickCheck
import Data.Monoid

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only x) (Only y) = Only $ x <> y

someFunc :: IO ()
someFunc = putStrLn "someFunc"

x :: Num a => Optional (Sum a)
x = Only (Sum 1) <> Only (Sum 2)

data Trivial = Trivial deriving (Eq, Show)

instance Monoid Trivial where
  mempty = Trivial
  mappend _ _ =  Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two x a) (Two y b) = Two (mappend x y) (mappend a b)

instance (Arbitrary a, Arbitrary b) =>Arbitrary (Two a b) where
  arbitrary = do
    x <-arbitrary
    y <-arbitrary
    return $ Two x y

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty
  mappend f g = Combine $ \x -> mappend (unCombine f x ) (unCombine g x)

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend mem1 mem2 = Mem $ \s ->let (a1, s1) = runMem mem1 s
                                     (a2, s2) = runMem mem2 s1
                                  in (mappend a1 a2, s2)
