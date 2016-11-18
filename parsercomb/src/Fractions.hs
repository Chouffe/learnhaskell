{-# LANGUAGE
   OverloadedStrings
  , QuasiQuotes #-}

module Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
import Text.RawString.QQ
import Text.Parser.Combinators

type NumberOrString = Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos = do
  skipMany (oneOf "\n")
  v <- (Left <$> integer) <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

main :: IO ()
main = do
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  -- print $ parseString parseFraction mempty badFraction
  print $ parseString parseNos mempty eitherOr
  print $ parseString (many parseNos) mempty eitherOr
