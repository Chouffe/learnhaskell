{-# LANGUAGE BangPatterns #-}

module Lib where

import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc3"

mfoldr k z [] =  z
mfoldr k z (x:xs) = k x $ mfoldr k z xs

c = foldr const 'z' ['a'..'e']

-- Outside In

hypo :: IO ()
hypo = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _ -> putStrLn "Hello"

hypo2 :: IO ()
hypo2 = do
  let x :: Int
      x = undefined
  s <- getLine
  case x `seq` s of  -- Can force evaluation everytime s is evaluated
    "hi" -> print x
    _ -> putStrLn "Hello"

hypo3 :: IO ()
hypo3 = do
  let x :: Int
      x = undefined
  s <- x `seq` getLine
  case s of
    "hi" -> print x
    _ -> putStrLn "Hello"

notGonnaHappenBru :: Int
notGonnaHappenBru =
   let x = undefined
       y = 2
       z = (x `seq` y `seq` 10, 11) in snd z

-- Exercises

-- const 1 undefined
-- (\x -> \_ -> x) 1 undefined
-- (\_ -> 1) undefined
-- 1

-- const undefined 1
-- (\x -> \_ -> x) undefined 1
-- (\_ -> undefined) 1
-- undefined

-- flip const undefined 1
-- flip :: (a -> b -> c) -> b -> a -> c
-- const :: a -> b -> a
-- flip const :: b -> a -> a
-- flip const = \_ -> \y -> y
-- (\_ -> \y -> y) undefined 1
-- (\y -> y) 1
-- 1

inc = (+1)
twice = inc . inc

homManyTimes =
  inc (trace "I got eval'd" (1 + 1))
    + twice (trace "I got eval'd" (1 + 1))

-- Promote sharing
x = trace "x" (1 :: Int)
y = trace "y" (1 :: Int)
z = x + y
-- will see x and y, different names prevent sharing

a = trace "a" (1 :: Int)
b = a + a
-- Will see a once, same name enables sharing

-- Indirection wont change this
u = trace "u" (1 :: Int)
v = id u + id u
-- Will see u once

-- Preventing sharing
-- Inlining expressions -> creates independent thunks that will get computed separately
--
q = undefined
r = "blah"

bott = do
  seq q $ print (snd (q, r))
