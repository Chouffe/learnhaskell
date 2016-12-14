module Queue where

import Criterion.Main (bench, defaultMain, nf, whnf, whnfIO)
import qualified Data.Sequence as S
import Data.Sequence ((<|), (|>))

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x q = Queue
  { enqueue = x : enqueue q
  , dequeue = dequeue q
  }

pop :: Queue a -> Maybe (a, Queue a)
pop q =
  case enqueue q of
    [] ->
      case dequeue q of
        []     -> Nothing
        (y:ys) -> Just (y, Queue { enqueue = [], dequeue = ys })
    xs ->
      case dequeue q of
        [] ->
          let (z:zs) = reverse xs
          in Just (z, Queue { enqueue = [], dequeue = zs })
        (y:ys) ->
          let zs = reverse xs
          in Just (y, Queue { enqueue = [], dequeue = ys ++ zs })

pushList :: a -> [a] -> [a]
pushList = (:)

popList :: [a] -> Maybe (a, [a])
popList [] =  Nothing
popList xs = let (y:ys) = reverse xs
             in Just (y, ys)

pushSequence :: a -> S.Seq a -> S.Seq a
pushSequence x s = s |> x

popSequence :: S.Seq a -> Maybe (a, S.Seq a)
popSequence s = if S.null s
                then Nothing
                else Just (S.index s 0, S.drop 1 s)

queuePushSeq :: Int -> Queue Int
queuePushSeq n = go n Queue { enqueue = [], dequeue = [] }
  where go 0 q = q
        go n q = go (n-1) (push n q)

queuePopSeq :: Int -> Queue Int -> Maybe (Queue Int)
queuePopSeq = go
  where go 0 q = Just q
        go n q = case pop q of
                   Nothing      -> Nothing
                   Just (x, q') -> go (n-1) q'

listPushSeq :: Int -> [Int]
listPushSeq n = go n []
  where go 0 xs = xs
        go n xs = go (n-1) (pushList n xs)

listPopSeq :: Int -> [Int] -> Maybe [Int]
listPopSeq = go
  where go 0 xs = Just xs
        go n xs = case popList xs of
                   Nothing      -> Nothing
                   Just (x, xs) -> go (n-1) xs

sequencePushSeq :: Int -> S.Seq Int
sequencePushSeq n = go n S.empty
  where go 0 xs = xs
        go n xs = go (n-1) (pushSequence n xs)

sequencePopSeq :: Int -> S.Seq Int -> Maybe (S.Seq Int)
sequencePopSeq = go
  where go 0 xs = Just xs
        go n xs = case popSequence xs of
                   Nothing      -> Nothing
                   Just (x, xs) -> go (n-1) xs

benchmarkQueue :: IO ()
benchmarkQueue = defaultMain
  [ bench "push/pop queue" $ whnf (queuePopSeq 10000 . queuePushSeq) 100000
  , bench "push/pop list" $ whnf (listPopSeq 1000 . listPushSeq) 100000
  , bench "push/pop sequence" $ whnf (sequencePopSeq 1000 . sequencePushSeq) 100000
  ]
