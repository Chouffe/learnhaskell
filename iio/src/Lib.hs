module Lib where

import GHC.Prim
import GHC.Types
import Control.Concurrent
import Data.Time.Clock
import Data.Time.Calendar
import Debug.Trace (trace)
import Control.Monad (join, replicateM)
import System.Random (randomIO, randomRIO)
import System.IO.Unsafe (unsafePerformIO)

newtype State s a
  = State { runState :: s -> (s, a) }

myData :: IO (MVar Int)
myData = newEmptyMVar

-- You should never do this. Bind it in a larger io action instead
-- Or use Reader/ReaderT
-- using unsagePerformIO is a way to reference to the same MVar
-- unsafePerformIO breaks referential transparency!! Do not use it in real code
myData2 :: MVar Int
myData2 = unsafePerformIO newEmptyMVar
{-# NOINLINE myData2 #-}

-- IO can be seen as a State with s ~ RealWorld
-- newtype IO a
--   = GHC.Types.IO (GHC.Prim.State# GHC.Prim.RealWorld
--                   -> (# GHC.Prim.State# GHC.Prim.RealWorld, a #))
        -- Defined in ‘GHC.Types’
        --
someFunc :: IO ()
someFunc = do
  t1 <- getCurrentTime
  print t1
  t2 <- getCurrentTime
  print t2

mvarPlayground :: IO ()
mvarPlayground = do
  mv <- myData
  putMVar mv 0
  mv' <- myData
  zero <- takeMVar mv'
  print zero

mvarPlayground2 :: IO ()
mvarPlayground2 = do
  mv <- newEmptyMVar
  putMVar mv (0 :: Int)
  zero <- takeMVar mv
  putMVar mv 1
  one <- takeMVar mv
  print (zero, one)

blah :: IO String
blah = return "blah"

blah' :: IO String
blah' = trace "outer trace" blah

woot :: IO String
woot = return $ trace "inner trace" "woot"

-- Will deadlock
ioSharingPlayground :: IO ()
ioSharingPlayground = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w

gimmeShelter :: Bool -> IO [Int]
gimmeShelter False = pure [0]
gimmeShelter True = replicateM 10 (randomRIO (0, 10))

-- Functor, Applicative, Monad
-- fmap :: (a -> b) -> IO a -> IO b

-- Functor
x :: IO Int
x = fmap (+1) randomIO

-- Applicative
y :: IO (String, String)
y = (,) <$> getLine <*> getLine

z :: IO Int
z = (+) <$> (randomIO :: IO Int) <*> (randomIO :: IO Int)

embedInIO :: a -> IO a
embedInIO = return

u :: IO Int
u = join $ embedInIO (embedInIO 1)

huehue :: IO (Either (IO Int) (IO ()))
huehue = do
  t <- getCurrentTime
  let (_, _, dayOfMonth) = toGregorian (utctDay t)
  if even dayOfMonth
  then return $ Left randomIO
  else return $ Right (putStrLn "no Soup for you")
