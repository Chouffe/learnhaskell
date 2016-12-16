module Vigenere where

import Data.Char

rotate :: Int -> Char -> Char
rotate n char = chr (mod (ord c - offset + m) 26 + offset)
  where m = mod n 26
        offset = ord 'a'
        c = toLower char

rot :: Int -> String -> String
rot n = foldr (\a b ->rotate n a : b) ""

caesar :: String -> String
caesar = rot 3

unCaesar :: String -> String
unCaesar = rot (26 - 3)

type Key = String

-- TODO: Abstract away rotation in vigenere
vigenere :: Key ->String -> String
vigenere k s = zipWith (\ck cs ->rotate (ord ck - offset) cs) repeatedKey s
  where offset = ord 'a'
        n = length s
        repeatedKey = take n $ concat $ repeat k

unVigenere :: Key -> String -> String
unVigenere k s = zipWith (\ck cs ->rotate (26 - (ord ck - offset)) cs) repeatedKey s
  where offset = ord 'a'
        n = length s
        repeatedKey = take n $ concat $ repeat k
