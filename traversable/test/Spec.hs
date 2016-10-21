import Control.Monad (liftM3)
import Data.Monoid
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers (quickBatch, EqProp, (=-=), eq)
import Test.QuickCheck.Classes (monoid, applicative, functor, traversable)

import Lib ( Identity(..)
           , Constant(..)
           , Optional(..)
           , List(..)
           , Three(..)
           , Three'(..)
           , Tree(..)
           )

-- Arbitrary instances

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = fmap  Constant arbitrary

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    frequency [ (1, return Nada)
              , (1, return $ Yep x)]

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [ (1, return Nil)
              , (1, return $ Cons x xs)]

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftM3 Three' arbitrary arbitrary arbitrary

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    x <- arbitrary
    l <- arbitrary
    r <- arbitrary
    frequency [ (1, return Empty)
              , (1, return $ Leaf x)
              , (1, return $ Node l x r)]

-- EqProp instances

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Eq a => EqProp (Constant a b) where (=-=) = eq

instance Eq a => EqProp (Optional a) where (=-=) = eq

instance Eq a => EqProp (List a) where (=-=) = eq

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance Eq a => EqProp (Tree a) where (=-=) = eq

-- Tests

main :: IO ()
main = do
  putStrLn "\nIdentity:"
  quickBatch (functor (undefined :: Identity (Int, Int, Int)))
  quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))


  putStrLn "\nConstant:"
  quickBatch (functor (undefined :: Constant Int (Int, String, String)))
  quickBatch (traversable (undefined :: Constant Int (Int, String, String)))

  putStrLn "\nOptional:"
  quickBatch (functor (undefined :: Optional (Int, String, String)))
  quickBatch (traversable (undefined :: Optional (Int, String, String)))

  putStrLn "\nList:"
  quickBatch (functor (undefined :: List (Int, String, String)))
  quickBatch (traversable (undefined :: List (Int, String, String)))

  putStrLn "\nThree:"
  quickBatch (functor (undefined :: Three Int String (Int, String, String)))
  quickBatch (traversable (undefined :: Three Int String (Int, String, String)))

  putStrLn "\nThree':"
  quickBatch (functor (undefined :: Three' Int (Int, String, String)))
  quickBatch (traversable (undefined :: Three' Int (Int, String, String)))

  putStrLn "\nTree:"
  quickBatch (functor (undefined :: Tree (Int, String, String)))
  quickBatch (traversable (undefined :: Tree (Int, String, String)))
