module Semigroup where

import Test.QuickCheck
import Data.Monoid

data Trivial = Trivial deriving (Eq, Show)

class Semigroup a where
  (<:>) :: a -> a -> a  -- Associativity

instance Semigroup Trivial where
  _ <:> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <:> (Identity y) = Identity $ x <:> y

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup b, Semigroup a) => Semigroup (Two a b) where
  (Two a x) <:> (Two b y) = Two (a <:> b) (x <:> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a x u) <:> (Three b y v) = Three (a <:> b) (x <:> y) (u <:> v)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <:> (BoolConj False) = BoolConj False
  (BoolConj False) <:> (BoolConj True) = BoolConj False
  _ <:> _  = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return $ BoolConj x

-- TODO
newtype Combine a b = Combine { unCombine :: a -> b }

instance (Num a, Semigroup a) => Semigroup (Sum a) where
  (Sum x) <:> (Sum y) = Sum $ x + y

instance Semigroup b => Semigroup (Combine a b) where
  f <:> g = Combine $ \x -> unCombine f x <:> unCombine g x

newtype Comp a =
  Comp { unComp :: a -> a }

instance Semigroup a => Semigroup (Comp a) where
  f <:> g = Comp $ unComp f . unComp g

data Validation a b =
    Fail a
  | Succ b
  deriving (Eq, Show)

instance Semigroup b => Semigroup (Validation a b) where
  Fail x <:> _ = Fail x
  _ <:> Fail x = Fail x
  (Succ x) <:> (Succ y) = Succ $ x <:> y

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Fail x) <:> AccumulateRight (Fail y) = AccumulateRight $ Fail y
  AccumulateRight (Fail x) <:> AccumulateRight (Succ y) = AccumulateRight $ Fail x
  AccumulateRight (Succ x) <:> AccumulateRight (Succ y) = AccumulateRight $ Succ y
