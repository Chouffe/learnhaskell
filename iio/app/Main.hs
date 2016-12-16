module Main where

import Lib
import Ini (ini, Config)
import Control.Monad (mapM)
import Vigenere (vigenere, unVigenere)
import System.IO (IOMode(ReadMode), getContents, hGetChar, hGetContents, hPutStr, hWaitForInput, openFile, stderr, stdin, stdout)
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Char (isLetter)
import qualified Data.Map.Strict as M
import System.Exit (exitSuccess, ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.Directory (getDirectoryContents)
import Text.Trifecta
import Text.Trifecta.Result

-- Vigenere command Line

data Mode = Decrypt | Encrypt
  deriving Show

getMode :: [String] -> Maybe Mode
getMode ["-d"] = Just Decrypt
getMode ["-e"] = Just Encrypt
getMode _      = Nothing

removeNonLetters :: String -> String
removeNonLetters = filter isLetter

usage :: String
usage = "Usage: iio [-de]"

version :: String
version = "Version 1.0.0"

main :: IO ()
main = do
  mode <- fmap getMode getArgs
  notTimeout <- hWaitForInput stdin 3000
  s <- fmap removeNonLetters getContents
  if notTimeout
  then exitWith (ExitFailure 1)
  else
    case mode of
      Nothing -> hPutStr stderr usage
      Just Decrypt -> hPutStr stdout $ unVigenere key s
      Just Encrypt -> hPutStr stdout $ vigenere key s
  where key = "mykey"

-- INI directory

iniFile :: FilePath -> Bool
iniFile "." = False
iniFile ".." = False
iniFile fp =
  case splitOn "." fp of
    []  -> False
    [x] -> False
    xs -> last xs == "ini"

configMap :: [FilePath] -> [String] -> M.Map FilePath (Text.Trifecta.Result.Result Ini.Config)
configMap fps xs = M.fromList $ zip fps parsedContents
  where parsedContents = map (parseString ini mempty) xs

iniDir :: String
iniDir = "data"

iniConfig :: IO ()
iniConfig = do
  putStrLn "INI directory: "
  iniDir <- getLine
  iniFilePaths <- filter iniFile <$> getDirectoryContents iniDir
  handles <- mapM (\fp -> openFile (iniDir ++ "/" ++ fp) ReadMode) iniFilePaths
  iniContents <- mapM hGetContents handles
  print $ configMap iniFilePaths iniContents
