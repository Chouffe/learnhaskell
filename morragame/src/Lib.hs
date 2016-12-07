module Lib where

import Control.Monad (replicateM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Maybe (fromMaybe)
import Data.List (unlines)
import System.Random

-- ADTs and models
type PlayerScore = Integer
data Score = Score PlayerScore PlayerScore deriving (Eq, Show)
data Mode = Odds | Evens deriving (Eq, Show)
data Player = Computer Mode | Human Mode deriving (Eq, Show)
type Fingers = Integer

instance Monoid Score where
  mempty = Score 0 0
  mappend (Score x1 y1) (Score x2 y2) = Score (x1 + x2) (y1 + y2)

-- TODO: Error Handling
check :: Integer -> Either String Integer
check x
  | x > 2 || x <= 0 = Left "Value should be 0 < x <= 2"
  | otherwise = Right x

-- TODO: deal with errors in an either type
winner :: (Player, Fingers)
       -> (Player, Fingers)
       -> Score
winner ((Human Odds), h) ((Computer Evens), c) =
  if odd (h + c)
     then Score 1 0 -- TODO: refactor logic
  else Score 0 1
winner ((Human Odds), h) ((Human Evens), c) =
  if odd (h + c)
  then Score 1 0
  else Score 0 1
winner ((Human Evens), h) ((Computer Odds), c) =
  if even (h + c)
  then Score 1 0
  else Score 0 1
winner _ _ = undefined

-- TODO: refactor logic
winnerShow :: (Player, Fingers)
           -> (Player, Fingers)
           -> String
winnerShow ((Human Odds), h) ((Computer Evens), c) =
  if odd (h + c)
  then "Player Human wins"
  else "Player Computer wins"
winnerShow ((Human Odds), h) ((Human Evens), c) =
  if odd (h + c)
  then "Player Human 1 wins"
  else "Player Human 2 wins"
winnerShow _ _ = undefined -- TODO

drawRandom :: RandomGen g => g -> IO (Integer, g)
drawRandom g = randomRIO (0, 1) >>= \x -> return $ (mod x 2, g)

-- TODO: add error handling and retries
drawHuman :: IO Integer
drawHuman = fmap ((flip mod 2) . read) getLine

introPlayer :: Integer ->  Player -> String
introPlayer x (Human Odds) = "Human " ++ show x ++ " is odds"
introPlayer x (Human Evens) = "Human " ++ show x ++ " is evens"
introPlayer x (Computer Odds) = "Computer" ++ show x ++ " is odds"
introPlayer x (Computer Evens) = "Computer" ++ show x ++ " is evens"

intro :: [Player] -> String
intro players = let labelPlayer   = "-- P is Player"
                    labelComputer = "-- C is Computer"
                    intros = zipWith introPlayer [0..] players
                 in unlines (labelPlayer:labelComputer:intros)


playerShoot :: RandomGen g => g -> Player -> IO (Integer, g)
playerShoot g (Computer _) = do
  (x, g1) <- drawRandom g
  putStrLn ("C: " ++ show x)
  return (x, g1)
playerShoot g (Human _) = do
  putStr "P: "
  x <- drawHuman
  return (x, g)

gameLoop :: (RandomGen g) => g
         -> Player
         -> Player
         -> Score
         -> IO ()
gameLoop g p1 p2 score = do
  (fingersP1, g1) <- playerShoot g p1
  (fingersP2, g2) <- playerShoot g p2
  let score' = winner (p1, fingersP1) (p2, fingersP2)
      newScore = mappend score score'
  putStrLn $ winnerShow (p1, fingersP1) (p2, fingersP2)
  putStrLn $ "Score: " ++ show newScore
  gameLoop g2 p1 p2 newScore

main :: IO ()
main =
  let players = [Human Odds, Human Evens] -- import System.Random
      [human, computer] = players
   in do
     g <- getStdGen
     putStr $ intro players
     gameLoop g human computer mempty

-- TODO:
----------------------------------------------------
-- add final score when breaks out of gameLoop
-- add error handling for incorrect inputs
-- add a StateT version and compare the differences
-- add tests with quickcheck and hspec
