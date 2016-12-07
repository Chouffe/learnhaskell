module Lib where

import Control.Monad (replicateM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Maybe (fromMaybe)
import Data.List (unlines)
import System.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type PlayerScore = Integer
type ComputerScore = Integer
data Score = Score PlayerScore ComputerScore deriving (Eq, Show)

instance Monoid Score where
  mempty = Score 0 0
  mappend (Score x1 y1) (Score x2 y2) = Score (x1 + x2) (y1 + y2)

-- TODO: Monoid instance for Score

data Mode = Odds | Evens deriving (Eq, Show)
data Player = Computer Mode | Human Mode deriving (Eq, Show)

type Game a = StateT Score IO a
type Fingers = Integer

-- Error Handling
check :: Integer -> Either String Integer
check x
  | x > 2 || x <= 0 = Left "Value should be 0 < x <= 2"
  | otherwise = Right x

play :: Integer -> Integer -> Score -> Score
play p1 p2 (Score psc csc)
  | even s = Score psc (csc + 1)
  | odd s = Score (psc + 1) csc
  where s = p1 + p2

-- TODO: deal with errors in an either type
winner :: (Player, Fingers) -> (Player, Fingers) -> Score
winner ((Human Odds), h) ((Computer Evens), c) =
  if odd (h + c)
  then Score 1 0
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

winnerShow :: (Player, Fingers) -> (Player, Fingers) -> String
winnerShow ((Human Odds), h) ((Computer Evens), c) =
  if odd (h + c)
  then "Player Human wins"
  else "Player Computer wins"
winnerShow ((Human Odds), h) ((Human Evens), c) =
  if odd (h + c)
  then "Player Human 1 wins"
  else "Player Human 2 wins"

drawRandom :: RandomGen g => g -> IO (Integer, g)
drawRandom g = randomRIO (0, 1) >>= \x -> return $ (mod x 2, g)

drawHuman :: RandomGen g => g -> IO (Integer, g)
drawHuman g = fmap ((\x -> (x,g)) . (flip mod 2) . read) getLine

introPlayer :: Integer ->  Player -> String
introPlayer x (Human Odds) = "Human " ++ show x ++ " is odds"
introPlayer x (Human Evens) = "Human " ++ show x ++ " is evens"
introPlayer x (Computer Odds) = "Computer" ++ show x ++ " is odds"
introPlayer x (Computer Evens) = "Computer" ++ show x ++ " is evens"

intro :: [Player] -> String
intro players = let labelPlayer = "-- P is Player"
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
  drawHuman g

gameLoop :: (RandomGen g) => g -> Player -> Player -> Score -> IO ()
gameLoop g p1 p2 score = do
  (fingersP1, _) <- playerShoot g p1
  (fingersP2, g2) <- playerShoot g p2
  putStrLn $ "FingersP1: " ++ show fingersP1
  putStrLn $ "FingersP2: " ++ show fingersP2
  let score' = winner (p1, fingersP1) (p2, fingersP2)
      newScore = mappend score score'
  putStrLn $ "Score: " ++ show newScore
  putStrLn $ winnerShow (p1, fingersP1) (p2, fingersP2)
  gameLoop g2 p1 p2 newScore

main :: IO ()
main =
  let players = [Human Odds, Human Evens] -- import System.Random
      [human, computer] = players
   in do
     g <- getStdGen
     putStr $ intro players
     gameLoop g human computer mempty
