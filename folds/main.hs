module Main where

main :: IO ()
main = putStrLn "Hello World"

mfoldr :: (a -> b -> b) -> b -> [a] -> b
mfoldr _ z [] = z
mfoldr f z (x:xs) = f x (mfoldr f z xs)

mfoldl :: (b -> a ->b) -> b -> [a] -> b
mfoldl _ z [] = z
mfoldl f z (x:xs) = mfoldl f (f z x) xs

mscanl :: (b -> a -> b) -> b -> [a] -> [b]
mscanl _ _ [] = []
mscanl f q (x:xs) = q : mscanl f (f q x) xs

mscanr :: (a -> b -> b) -> b -> [a] -> [b]
mscanr _ _ [] = []
mscanr f q (x:xs) = map (f x) $ mscanr f q xs ++ [q]

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

factorial :: Integer ->  Integer
factorial n = scanl (*) 1 [1..] !! (fromInteger n)
