module Lib where

import Control.Monad.Primitive
import Control.Monad.ST
import Criterion.Main (bench, defaultMain, nf, whnf, whnfIO)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM
import Debug.Trace
import qualified Data.Sequence as SEQ

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

lists :: [[Int]]
lists = replicate 10 [1..100000]

seqs :: [SEQ.Seq Int]
seqs = replicate 10 (SEQ.fromList [1..100000])

seqBenchmark :: IO ()
seqBenchmark = defaultMain
  [ bench "concatenate lists" $
    nf mconcat lists
  , bench "concatenate sequences" $
    nf mconcat seqs
  , bench "indexing lists" $
    whnf (!! 9001) lists
  -- , bench "indexing sequences" $
  --   whnf (`SEQ.index` 9001) seqs
  ]

-- Vectors
slice :: Int -> Int -> [a] -> [a]
slice from len = take len . drop from

benchmarkVectors :: IO ()
benchmarkVectors = defaultMain
  [ bench "slice lists" $
    whnf (head . slice 100 900) [1..10000]
  , bench "slice vectors" $
    whnf (V.head . V.slice 100 900) (V.fromList [1..10000])
  ]

-- Mutable vectors

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n + 1)
  go n mvec
    where go 0 v = return v
          go n v = MV.write v n 0 >> go (n-1) v

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n + 1)
  go n mvec
    where go 0 v = V.freeze v
          go n v = MV.write v n 0 >> go (n-1) v

benchmarkMutableVectors :: IO ()
benchmarkMutableVectors = defaultMain
  [ bench "mutable IO vector" $
    whnfIO (mutableUpdateIO 9999998)
  , bench "mutable ST vector" $
    whnf mutableUpdateST 9999998]

