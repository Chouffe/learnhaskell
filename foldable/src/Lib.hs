module Lib where

import Data.Monoid

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Library functions

sum :: (Foldable t, Num a) => t a -> a
sum   = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem:: (Foldable t, Eq a) => a -> t a -> Bool
elem e = getAny . foldMap (Any . (== e))

-- TODO: use monoids and foldMap instead
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (\x r -> case r of
                           Nothing -> Just x
                           Just y -> if x <= y then Just x else Just y) Nothing

null :: (Foldable t) => t a -> Bool
null = getAll . foldMap (\_ -> All False)

length :: (Foldable t) => t a -> Int
length = getSum . foldMap (\_ -> Sum 1)

toList :: (Foldable t) => t a -> [a]
toList = foldMap (:[])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

mfoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
mfoldMap f = foldr (\x y -> f x `mappend` y) mempty

-- Chapter Exercises

-- Definition:
--
-- class Foldable (t :: * -> *) where
--   fold :: Monoid m => t m -> m
--   foldMap :: Monoid m => (a -> m) -> t a -> m

data Constant a b =
  Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap f (Two _ y) = f y

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ y z) = f y <> f z

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ x y z) = f x <> f y <> f z

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p = foldMap (\x -> if p x then pure x else mempty)
