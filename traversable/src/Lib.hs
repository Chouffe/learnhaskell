module Lib where

import Data.Monoid
import Control.Applicative

-- Definition:
--
-- class (Functor t, Foldable t) => Traversable t where
--   {-# MINIMAL traverse | sequenceA #-}
--         -- | Map each element of a structure to an action,
--         -- evaluate these actions from left to right, and
--         -- collect the results. For a version that ignores
--         -- the results see 'Data.Foldable.traverse_'.
--         traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--         traverse f = sequenceA . fmap f

--         -- | Evaluate each action in the structure from
--         -- left to right, and collect the results.
--         -- For a version that ignores the results see
--         -- 'Data.Foldable.sequenceA_'.
--         sequenceA :: Applicative f => t (f a) -> f (t a)
--         sequenceA = traverse id

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ x = Constant $ getConstant x

instance Foldable (Constant a) where
  foldMap f x = mempty

instance Traversable (Constant a) where
  traverse _ = pure . Constant . getConstant

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = fmap Cons (f x) <*> traverse f xs

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = fmap (Three x y) (f z)

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y <> f z

instance Traversable (Three' a) where
  traverse f (Three' x y z) = liftA2 (Three' x) (f y) (f z)

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node l val r) = Node (fmap f l) (f val) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = fmap Leaf $ f x
  traverse f (Node l x r) = liftA3  Node (traverse f l) (f x) (traverse f r)
