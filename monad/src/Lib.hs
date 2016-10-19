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
