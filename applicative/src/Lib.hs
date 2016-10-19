module Lib where

import Data.Monoid
import Control.Applicative

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ c = Constant $ getConstant c

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  c1 <*> c2 = let x = getConstant c1
                  y = getConstant c2
             in Constant $ x <> y

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person =
  Person Name Address
  deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
     then Nothing
     else Just s

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = fmap Address $ validateLength 100 s

mkPerson :: String -> String -> Maybe Person
mkPerson n a = liftA2 Person (mkName n) (mkAddress a)

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure x) =  Failure x
  fmap f (Success x) = Success $ f x

instance Monoid e => Applicative (Validation e) where
  pure x = Success x
  (Failure e1) <*> (Failure e2) = Failure (e1 `mappend` e2)
  Failure e <*> Success _ = Failure e
  Success _ <*> Failure e = Failure e
  Success f <*> Success y = Success $ f y
