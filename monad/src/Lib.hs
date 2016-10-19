module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap  _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

data PhhhbbtttEither b a
  = L a
  | R b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither a) where
  fmap f (L x) = L $ f x
  fmap _ (R x) = R x

instance Applicative (PhhhbbtttEither a) where
  pure = L
  (R x) <*> _ = R x
  (L _) <*> (R x) = R x
  (L f) <*> (L x) = L $ f x

instance Monad (PhhhbbtttEither a) where
  return = pure
  (R x) >>= _ = R x
  (L x) >>= f = f x

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap  f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  fs <*> xs = flatMap (`fmap` xs) fs

instance Monoid (List a) where
  mempty = Nil
  mappend = append

instance Monad List where
  return = pure
  xs >>= f = flatMap f xs

-- Monad functions

j :: Monad m => m (m a) -> m a
j mma = mma >>=  id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f mx my = do
  x <- mx
  y <- my
  return $ f x y

a :: Monad m => m a -> m (a -> b) -> m b
a mx mf = mf >>= \f -> l1 f mx

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = do
  y <- f x
  ys <- meh xs f
  return (y:ys)

flipType :: (Monad m) => [m a] -> m [a]
flipType xms = meh xms id
