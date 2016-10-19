module Main where

import Data.Maybe (fromMaybe)
import Data.Char (toLower)

main :: IO ()
main = putStrLn "Hello"

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

-- countTheBeforeVowel :: String -> Integer
-- countTheBeforeVowel s = go 0 (words s)
--   where go acc [] = acc
--         go acc [_] = acc
--         go acc (x:(y@(y0:ys)):zs)
--           | x == "the" && isVowel y0  = go (acc + 1) zs
--           | otherwise = go acc (y:zs)

isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiou"

myLength :: Foldable t => t a -> Integer
myLength = foldr (\_ b -> b + 1) 0

countVowels :: String -> Integer
countVowels = myLength . filter isVowel

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = let (vs, cs) = span isVowel s
            in if length vs >= length cs
                  then Nothing
                  else Just $ Word' s

-- Natural numbers

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x == 0 = Just Zero
  | x > 0 = integerToNat (x - 1) >>= Just . Succ
  | otherwise = Nothing

-- Lib for Maybe

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- Catamorphism
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just x) = f x

mFromMaybe :: a -> Maybe a -> a
mFromMaybe x = mayybee x id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

mCatMaybes :: [Maybe a] -> [a]
mCatMaybes = concatMap maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (x:xs) = do
  y <- x
  ys <- flipMaybe xs
  return $ y : ys

-- Lib for either

lefts :: [Either a b] -> [a]
lefts = foldr (\a b -> case a of
                            Left x -> x:b
                            _ -> b) []

rights :: [Either a b] -> [b]
rights = foldr (\a b -> case a of
                        Right x -> x:b
                        _ -> b) []

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers es = (lefts es, rights es)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe _ (Left _) = Nothing
eitherMaybe f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f = either' (const Nothing) (Just . f)

-- Unfolds

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Nothing -> []
                  Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
               Nothing -> Leaf
               Just (l, val, r) -> (Node (unfold f l) val (unfold f r))

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> case x of
                              0 -> Nothing
                              _ -> Just (x-1, x, x-1)) n
