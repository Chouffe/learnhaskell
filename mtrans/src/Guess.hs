module Guess where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map               as Map
import           Data.Maybe
import           System.Random

type GuessState = Integer -- Number of attempts

-- Simple Monad Transformer
-- StateT with IO

guessSession :: Integer -> StateT GuessState IO ()
guessSession answer = do
  liftIO $ putStr "Make a guess: "
  guess <- liftIO getLine
  let g = read guess :: Integer
  case compare g answer of
    LT -> do
      liftIO $ putStrLn "Too Low"
      modify (+1)
      guessSession answer
    GT -> do
      liftIO $ putStrLn "Too High"
      modify (+1)
      guessSession answer
    EQ -> liftIO $ putStrLn "Got it!"


game :: IO ()
game = do
  answer <- randomRIO ((1,100) :: (Integer, Integer))
  putStrLn "I am thinking of a number between 1 and 100"
  attempts <- execStateT (guessSession answer) 1
  putStrLn $ "Success in " ++ show attempts ++ " attempts"

-- Combining Monad Transformers
-- ReaderT, StateT and IO

guessSession2 :: ReaderT Integer (StateT GuessState IO) ()
guessSession2 = do
  answer <- ask
  liftIO $ putStr "Make a guess: "
  guess <- liftIO getLine
  let g = read guess :: Integer
  liftIO $ putStrLn $ "Your guess is: " ++ guess
  case compare g answer of
    LT -> do
      liftIO $ putStrLn "Too Low"
      lift $ modify (+1)
      guessSession2
    GT -> do
      liftIO $ putStrLn "Too High"
      lift $ modify (+1)
      guessSession2
    EQ -> liftIO $ putStrLn "Got it!"
game2 :: IO ()
game2 = do
  answer <- randomRIO ((1,100) :: (Integer, Integer))
  putStrLn "I am thinking of a number between 1 and 100"
  attempts <- execStateT (runReaderT guessSession2 answer) 1
  putStrLn $ "Success in " ++ show attempts ++ " attempts"
