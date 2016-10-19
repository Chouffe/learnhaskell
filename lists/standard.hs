module Standard where

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: Foldable t => (a -> Bool) -> t a -> [a]
myFilter p = foldr (\a b -> if p a then a : b else b) []

myAnd :: [Bool] -> Bool
-- Direct recursion
-- myAnd [] = True
-- myAnd (x:xs) = x && myAnd xs
-- Fold
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
-- Fold
-- myAny p = foldr (\x r -> r || p x) False
-- Map
myAny p = myOr . map p

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (==x)

myReverse :: [a] -> [a]
-- myReverse = foldl (flip (:)) []
myReverse = foldr (\a b -> b ++ [a]) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
