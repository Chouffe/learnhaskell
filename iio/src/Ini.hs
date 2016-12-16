{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ini (ini, Config) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
-- parsers 0.12.3, trifecta 1.5.2
import Text.Trifecta
import Control.Monad (void)

headerEx :: String
headerEx = "[blah]"

headerEx' :: ByteString
headerEx' = "[blah]"

assignmentEx :: String
assignmentEx = "woot=1"

commentEx :: String
commentEx = "; Last modified 1 April 2001 by Chuck Norris"

sectionEx :: String
sectionEx = [r|
; ignore me
[section]
host=wikipedia.org
alias=claw

[sectiontwo]
host=google.com
alias=g
|]

newtype Header =
  Header String
  deriving (Eq, Ord, Show)

type Name = String
type Value = String
type Assignments = Map Name Value

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)


bracketPair :: Parser a -> Parser a
bracketPair p = char '[' *> p <* char ']'
header :: Parser Header
header = bracketPair (Header <$> some letter)

-- Skip end of line and whitespace beyond.
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

assignment :: Parser (Name, Value)
assignment = do
  name <- some letter
  char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

comment :: Parser String
comment = do
  char ';' <|> char '#'
  s <- many (noneOf "\n")
  skipEOL
  return s

skipComments :: Parser ()
skipComments = void (skipMany comment)

skipWhitespace :: Parser ()
skipWhitespace = skipMany $ char ' ' <|> char '\n'

section :: Parser Section
section = do
  skipWhitespace
  skipComments
  h <- header
  skipEOL
  as <- some assignment
  return $ Section h (M.fromList as)

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) = M.insert h a

ini :: Parser Config
ini = do
  xs <- some section
  return $ Config $ foldr rollup M.empty xs
