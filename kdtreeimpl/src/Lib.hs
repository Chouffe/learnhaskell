module Lib where

import Data.List (sort, sortBy)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Point = Point { x :: Integer
                   , y :: Integer
                   , z :: Integer}

depthCompare :: Integer -> Point ->Point -> Ordering
depthCompare depth (Point x1 y1 z1) (Point x2 y2 z2)
  | mod depth 3 == 0 = compare x1 x2
  | mod depth 3 == 1 = compare y1 y2
  | mod depth 3 == 2 = compare z1 z2

median :: [Point] ->Integer -> Maybe Point
median [] _ = Nothing
median points depth = Just $ sortBy (depthCompare depth) points !! (div n 2)
  where n = length points


data Tree = Empty
          | Leaf Point
          | Node Tree Point Tree

kdtree :: [Point] -> Integer ->Tree
kdtree [] _ = Empty
kdtree [point] _ =  Leaf point
kdtree points depth = undefined

