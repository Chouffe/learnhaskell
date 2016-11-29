module Fraction where

import Control.Applicative
import Data.Ratio ((%))

import Text.Trifecta
import Text.Parser.Combinators (eof)


type FractionOrFloat = Either Rational Float

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

toInt :: [Integer] -> Integer
toInt = sum . zipWith (\k x -> x * 10 ^ k) [0..] . reverse

fraction :: Parser Rational
fraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator should not be zero"
    _ -> return (numerator % denominator)

-- Wrong implementation
float :: Parser Float
float = do
  p <- many decimal
  char '.'
  q <- many decimal
  return $ (fromIntegral (toInt p)) + (fromIntegral (toInt q)) * 1.0 / (10.0 ^ (fromIntegral (1 + length q)))

fractionOrFloat :: Parser FractionOrFloat
fractionOrFloat = try
      (Left <$> fraction)
  <|> (Right <$> float)

output:: IO ()
output = do
  print $ parseString fraction mempty shouldWork
