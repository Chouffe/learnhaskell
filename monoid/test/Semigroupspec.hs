module Semigroupspec where

import Test.QuickCheck
import Semigroup
  ( Semigroup
  , Trivial
  , Identity
  , Two
  , Three
  , BoolConj
  , (<:>))

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <:> (b <:> c) == (a <:> b) <:> c

main :: IO ()
main = do
        quickCheck (semigroupAssoc :: Trivial ->  Trivial ->  Trivial -> Bool)
        quickCheck (semigroupAssoc :: Identity Trivial ->  Identity Trivial -> Identity Trivial -> Bool)
        quickCheck (semigroupAssoc :: Two (Identity Trivial) Trivial ->  Two (Identity Trivial) Trivial -> Two (Identity Trivial) Trivial -> Bool)
        quickCheck (semigroupAssoc :: Three Trivial Trivial Trivial -> Three Trivial Trivial Trivial -> Three Trivial Trivial Trivial -> Bool)
        quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
