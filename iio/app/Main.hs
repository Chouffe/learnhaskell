module Main where

import Lib
import Control.Monad (mapM)
import Vigenere (vigenere, unVigenere)
import System.IO (getContents, hGetChar, hPutStr, hWaitForInput, stderr, stdin, stdout)
import System.Environment (getArgs)
import Data.Char (isLetter)
import System.Exit (exitSuccess, ExitCode(ExitFailure, ExitSuccess), exitWith)

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
