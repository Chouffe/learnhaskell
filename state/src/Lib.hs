module Lib where

import Control.Monad
import Control.Monad.Trans.State

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi st) = Moi $ \s -> let (a, s1) = st s in (f a, s1)

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  (Moi f) <*> (Moi st) = Moi $ \s -> let (fab, s1) = f s
                                         (a, s2) = st s1
                                      in (fab a, s2)

instance Monad (Moi s) where
  return = pure
  (Moi st) >>= f = Moi $ \s -> let (a, s1) = st s
                                   st2 = (runMoi $ f a)
                                in st2 s1

-- Chapter Exercises

mget :: Moi s s
mget = Moi $ \s -> (s, s)

mput :: s -> Moi s ()
mput s = Moi $ \x -> ((), s)

mexec :: Moi s a -> s -> s
mexec (Moi sa) s = snd $ sa s

meval :: Moi s a -> s -> a
meval (Moi sa) s = fst $ sa s

mmodify :: (s -> s) -> Moi s ()
mmodify f = Moi $ \s ->((), f s)

-- FizzBuzz exercise

fizzBuzz :: Integer -> String
fizzBuzz n
  | mod n 15 == 0 = "FizzBuzz"
  | mod n 5 == 0 = "Buzz"
  | mod n 3 == 0 = "Fizz"
  | otherwise = show n

solution :: IO ()
solution =
  mapM_ (putStrLn . fizzBuzz) [1..100]

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)


