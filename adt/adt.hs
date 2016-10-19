{-# LANGUAGE FlexibleInstances #-}
module ADT where

import Data.Int

newtype Goats = Goats Int deriving Show

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Goats where
  tooMany (Goats n) = n > 43

instance (Num a, Ord a) => TooMany (a, String) where
  tooMany (n, _) = n > 43

instance (Num a, Ord a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = (x + y) > 42

instance TooMany Integer where
  tooMany n = n > 10

data NumberOrBool =
  Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node b1 y b2)
  | x == y = Node b1 x b2
  | x < y = Node (insert x b1) y b2
  | x > y = Node b1 y (insert x b2)
insert _ _ = Leaf -- Should not be necesary for the compiler

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node l x r) =
  Node (mapTree f l) (f x) (mapTree f r)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l x r) = [x] ++ preorder l ++ preorder r

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l x r) = postorder l ++ postorder r ++ [x]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z b = foldr f z (inorder b)
