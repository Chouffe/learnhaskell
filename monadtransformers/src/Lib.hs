{-# LANGUAGE InstanceSigs, FlexibleContexts #-}

module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Identity a =
  Identity { runIdentity :: a }

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose x) = Compose $ fmap (<*>) f <*> x

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: (Monoid m, Foldable (Compose f g)) => (a -> m) -> (Compose f g) a -> m
  foldMap f (Compose xs) = foldMap (foldMap f) xs

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
  traverse f (Compose t) = Compose <$> traverse (traverse f) xs
