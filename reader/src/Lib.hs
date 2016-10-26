module Lib where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Maybe (fromMaybe)

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs = lookup 3 $ zip x y

ys = lookup 6 $ zip y z

zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2(,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)

v = fold $ map All (sequA 6)

u = sequA $ fromMaybe 0 s'
