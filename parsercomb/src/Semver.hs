module Semver where

import Control.Applicative

import Text.Trifecta.Result (Result(..))
import Text.Trifecta
  (some, letter, integer, char, Parser, symbol)
import Text.Parser.Combinators (eof, sepBy, try, option, skipOptional)

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]
data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  SemVer x1 y1 z1 _ _ <= SemVer x2 y2 z2 _ _
    | x1 < x2 = True
    | x1 > x2 = False
    | y1 < y2 = True
    | y1 > y2 = False
    | z1 <= z2 = True
    | z1 > z2 = False

numberOrString :: Parser NumberOrString
numberOrString =
      NOSS <$> some letter
  <|> NOSI <$> integer

release :: Parser Release
release = sepBy numberOrString (symbol ".")

metadata :: Parser Metadata
metadata = sepBy numberOrString (symbol ".")

semVer :: Parser SemVer
semVer = do
  major <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer
  skipOptional (char '-')
  r <- option [] release
  skipOptional (char '+')
  m <- option [] metadata
  return $ SemVer major minor patch r m
