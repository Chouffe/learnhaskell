module Digits where

import Control.Applicative

import Text.Trifecta
  (some, letter, integer, char, Parser, symbol)
import Text.Parser.Combinators
  (eof, sepBy, try, option, skipOptional)

mDigit :: Parser Char
mDigit =
      char '0'
  <|> char '1'
  <|> char '2'
  <|> char '3'
  <|> char '4'
  <|> char '5'
  <|> char '6'
  <|> char '7'
  <|> char '8'
  <|> char '9'

chartoInteger :: Char -> Maybe Integer
chartoInteger '0' = Just 0
chartoInteger '1' = Just 1
chartoInteger '2' = Just 2
chartoInteger '3' = Just 3
chartoInteger '4' = Just 4
chartoInteger '5' = Just 5
chartoInteger '6' = Just 6
chartoInteger '7' = Just 7
chartoInteger '8' = Just 8
chartoInteger '9' = Just 9
chartoInteger _ = Nothing

toNum :: Num a => [a] -> a
toNum = sum . zipWith (\k x -> x * 10 ^ k) [0..] . reverse

base10Integer :: Parser Integer
base10Integer = do
  signum <- option '+' (char '-')
  ds <- many mDigit
  let ids = sequence $ fmap chartoInteger ds
  case ids of
    Nothing -> fail "Expected digits"
    Just xs -> case xs of
                 [] -> fail "Expecting digits"
                 ys -> let result = toNum ys
                        in case signum of
                             '-' -> return $ - result
                             _ -> return result
