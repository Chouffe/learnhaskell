module Lib where

import Criterion.Main (bench, defaultMain, nf, whnf)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

{-# INLINABLE (!?) #-}
infixl 9 !?

(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n - 1)

{-# INLINABLE (!!?) #-}
infixl 9 !!?
(!!?) :: [a] -> Int -> Maybe a
xs !!? n
  | n < 0 = Nothing
  | otherwise =
      foldr (\x r k -> case k of
                         0 -> Just x
                         _ -> r (k-1)) (const Nothing) xs n


myList :: [Int]
myList =
  trace "myList was evaluated"
    ([1..9999] ++ [undefined])

benchmark :: IO ()
benchmark = defaultMain
  [ bench "index list 9999"
    $ whnf (myList !!) 9998
  , bench "index list maybe index 9999"
    $ nf (myList !?) 9998
  , bench "index list maybe index 2 9999"
    $ nf (myList !!?) 9998
  ]


-- Basic datastructures
bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 1000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 1000 stream
  where stream = iterate (+1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

genList :: Int -> [(String, Int)]
genList n = map (\x -> (show x, x)) [0..n]

pairList :: [(String, Int)]
pairList = genList 90001

testMap :: M.Map String Int
testMap = M.fromList pairList

mapBenchmark :: IO ()
mapBenchmark = defaultMain
  [ bench "Regular List"
    $ whnf (lookup "does not exit") pairList
  , bench "Maps"
    $ whnf (M.lookup "does not exit") testMap
  ]

setBenchmark :: IO ()
setBenchmark = defaultMain
  [ bench "Maps member"
  $ whnf membersMap 9999
  , bench "Sets member"
  $ whnf membersSet 9999
  ]
