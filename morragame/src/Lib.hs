module Lib where

import Control.Monad (replicateM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Maybe (fromMaybe)
import Data.List (unlines, zipWith4)
import qualified Data.Map as M
import System.Random

-- ADTs and models
type PlayerScore = Integer
data Score = Score PlayerScore PlayerScore deriving (Eq, Show)
data Mode = Odds | Evens deriving (Eq, Show)
data Player = Computer Mode | Human Mode deriving (Eq, Show)
type Fingers = Integer

type Memory = M.Map (Shoot, Shoot, Shoot) Shoot
type Shoot = Integer

data GameState = GameState { shootSequence :: [Shoot]
                           , score :: Score
                           } deriving (Eq, Show)

instance Monoid Score where
  mempty = Score 0 0
  mappend (Score x1 y1) (Score x2 y2) = Score (x1 + x2) (y1 + y2)

-- TODO: monoid instance for GameState

memory :: [Shoot] -> Memory
memory [] = M.empty
memory [x] = M.empty
memory [x, y] = M.empty
memory [x, y, z] = M.empty
memory xs@(x:y:z:u:rs) = M.fromList $ zipWith4 (\a b c d -> ((a, b, c), d)) xs (y:z:u:rs) (z:u:rs) (u:rs)

nextShoot :: [Shoot] -> Maybe Shoot
nextShoot xs
  | n <= 3 = Nothing
  | otherwise = M.lookup (tuplify3 (drop (n - 3) xs)) memo
  where n = length xs
        memo = memory xs
        tuplify3 [x, y, z] = (x, y, z)

check :: Integer -> Either String Shoot
check x
  | x > 2 || x <= 0 = Left "Shoot value should be 0 < x <= 2"
  | otherwise = Right x

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

-- TODO: do we need to pass around the random generators?
drawRandom :: RandomGen g => g -> IO (Shoot, g)
drawRandom g = randomRIO (0, 1) >>= \x -> return $ (mod x 2, g)

drawMemoryOrRandom :: RandomGen g => Mode -> g -> [Shoot] -> IO (Shoot, g)
drawMemoryOrRandom mode g xs =
  case (nextShoot xs, mode) of
    (Just 0, Odds) -> return (1, g)
    (Just 1, Odds) -> return (0, g)
    (Just 0, Evens) -> return (0, g)
    (Just 1, Evens) -> return (1, g)
    _ -> drawRandom g


getLineInteger :: IO Integer
getLineInteger = do
  l <- getLine
  case (reads l :: [(Shoot, String)]) of
    [] -> putStrLn "Please enter an Integer.." >> getLineInteger
    [(x, _)] -> return x

drawHuman :: IO Shoot
drawHuman = do
  x <- getLineInteger
  case check x of
    Right x -> return x
    Left e -> putStrLn e >> drawHuman

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


playerShoot :: RandomGen g => g -> Player -> IO (Shoot, g)
playerShoot g (Computer _) = do
  (x, g1) <- drawRandom g
  putStrLn ("C: " ++ show x)
  return (x, g1)
playerShoot g (Human _) = do
  putStr "P: "
  x <- drawHuman
  return (x, g)

playerShoot2 :: RandomGen g => g -> GameState -> Player -> IO (Shoot, g)
playerShoot2 g gameState (Computer mode) = do
  (x, g1) <- drawMemoryOrRandom mode g (shootSequence gameState)
  putStrLn ("C: " ++ show x)
  return (x, g1)
playerShoot2 g _ (Human _) = do
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

gameLoop2 :: Player
          -> Player
          -> StateT GameState IO ()
gameLoop2 p1 p2 = do
  g <- liftIO getStdGen
  gameState <- get
  (fingersP1, g1) <- liftIO $ playerShoot2 g gameState p1
  (fingersP2, g2) <- liftIO $ playerShoot2 g gameState p2
  let score' = winner (p1, fingersP1) (p2, fingersP2)
  let oldScore = score gameState
  let newScore = mappend oldScore score'
  let oldSequence = shootSequence gameState
  liftIO $ putStrLn $ winnerShow (p1, fingersP1) (p2, fingersP2)
  liftIO $ putStrLn $ "Score: " ++ show newScore
  put (GameState {shootSequence=oldSequence ++ [fingersP1], score=newScore})
  gameLoop2 p1 p2

main2 :: IO ()
main2 =
  let players = [Human Odds, Computer Evens] -- import System.Random
      [human, computer] = players
   in do
     putStr $ intro players
     runStateT (gameLoop2 human computer) GameState {score=mempty, shootSequence=[]}
     return ()

-- TODO:
----------------------------------------------------
-- add tests with quickcheck and hspec
