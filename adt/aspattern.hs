module Aspattern where

import Data.Char (toUpper)
import Data.String (unwords
                   , words)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ =  True
isSubsequenceOf _ []  = False
isSubsequenceOf zs@(x:xs) (y:ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf zs ys

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

-- splitSentences :: String -> String
-- splitSentences s = go [] s
--   where go acc [] = reverse acc
--         go (a:as) (x:xs)
--           | x == '.'  = go (a ++ ['.'] : as) xs

-- capitalizeSentence :: String -> String
-- capitalizeSentence [] = []
-- capitalizeSentence ys = map (\s ->let (x:xs) = words s in unwords $ capitalizeWord x : xs) sentences
--   where sentences = splitSentences ys
