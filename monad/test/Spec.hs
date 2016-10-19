import Data.Monoid
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers (verboseBatch, quickBatch, EqProp, (=-=), eq)
import Test.QuickCheck.Classes (monoid, applicative, functor, monad)
import Lib (Nope(..), PhhhbbtttEither(..), Identity(..), List(..))

instance Eq a => EqProp (Nope a) where (=-=) = eq

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where (=-=) = eq

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Eq a => EqProp (List a) where (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return $ L x)
              , (1, return $ R y)
              ]

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [ (1, return Nil)
              , (1, return (Cons x xs))
              ]

main :: IO ()
main = do
  putStrLn "\nFunctor:"
  quickBatch $ functor (undefined :: Nope (String, String, Integer))
  quickBatch $ functor (undefined :: PhhhbbtttEither String (String, String, Integer))
  quickBatch $ functor (undefined :: Identity (String, String, Integer))
  quickBatch $ functor (undefined :: List (String, String, Integer))

  putStrLn "\nApplicative:"
  quickBatch $ applicative (undefined :: Nope (String, String, Integer))
  quickBatch $ applicative (undefined :: PhhhbbtttEither String (String, String, Integer))
  quickBatch $ applicative (undefined :: Identity (String, String, Integer))
  quickBatch $ applicative (undefined :: List (String, String, Integer))

  putStrLn "\nMonad:"
  quickBatch $ monad (undefined :: Nope (String, String, Integer))
  quickBatch $ monad (undefined :: PhhhbbtttEither String (String, String, Integer))
  quickBatch $ monad (undefined :: Identity (String, String, Integer))
  quickBatch $ monad (undefined :: List (String, String, Integer))

  putStrLn "\nmonoid:"
  quickBatch $ monoid (undefined :: List (String, String, Integer))
