module Lib where

import Text.Trifecta
import Text.Parser.Combinators (eof, choice)
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

onetwothreeOronetwoOrone :: Parser String
-- onetwothreeOronetwoOrone = choice [ string "123"
--                                   , string "12"
--                                   , string "1"
--                                   ]

onetwothreeOronetwoOrone = choice [p3, p2, p1]
  where p1 = char '1' >> char '2' >> char '3' >> eof >> return "123"
        p2 = char '1' >> char '2' >> eof >> return "12"
        p3 = char '1' >> eof >> return "1"

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
