import Data.Monoid
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers (quickBatch, EqProp, (=-=), eq)
import Test.QuickCheck.Classes (monoid, applicative, functor, monad)
import Lib (Nope(..))

instance Eq a => EqProp (Nope a) where (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

main :: IO ()
main = do
  putStrLn "\nFunctor:"
  quickBatch $ functor (undefined :: Nope (String, String, Integer))

  putStrLn "\nApplicative:"
  quickBatch $ applicative (undefined :: Nope (String, String, Integer))
  putStrLn "\nMonad:"
  quickBatch $ monad (undefined :: Nope (String, String, Integer))
