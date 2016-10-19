module Monoidspec where

import Data.Monoid
import Test.QuickCheck
import Lib (Trivial, Two)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId a = mempty <> a == a

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId a = a <> mempty == a

main :: IO ()
main = do
        quickCheck (monoidAssoc :: String -> String -> String -> Bool)
        quickCheck (monoidLeftId :: String -> Bool)
        quickCheck (monoidRightId :: String -> Bool)
        quickCheck (monoidLeftId :: Trivial -> Bool)
        quickCheck (monoidAssoc :: Two String Trivial -> Two String Trivial -> Two String Trivial -> Bool)
