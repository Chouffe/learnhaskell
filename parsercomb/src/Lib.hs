module Lib where

import Text.Trifecta
import Text.Parser.Combinators (eof)
import Control.Applicative

someFunc :: IO ()
someFunc = putStrLn "someFunc"

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

two :: Parser Char
two = char '2'

three :: Parser Char
three = char '3'

onetwothree :: Parser String
onetwothree = do
  o <- one
  t <- two
  tt <- three
  return $ [o, t, tt]



one' :: Parser a
one' = one >> stop

testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"

testParseUnit :: Parser () -> IO ()
testParseUnit p =
    print $ parseString p mempty "123"

test = do
  putStrLn "one"
  testParse one
  putStrLn "two"
  testParse two
  putStrLn "one >> two"
  testParse $ one >> two
  putStrLn "one >> two >> three"
  testParse $ one >> two >> three
  putStrLn "one >> two >> three >> eof"
  testParseUnit $ one >> two >> three >> eof
  putStrLn "one >> two >> eof"
  testParseUnit $ one >> two >> eof
