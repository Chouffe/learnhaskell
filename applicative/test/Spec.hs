import Data.Monoid
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers (quickBatch, EqProp, (=-=), eq)
import Test.QuickCheck.Classes (monoid, applicative, functor)
import Lib (Validation(Failure, Success))

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = (Cons x Nil)
  Nil <*> _ = Nil
  fs <*> xs = flatMap (\f -> fmap f xs) fs

instance Monoid (List a) where
  mempty = Nil
  mappend = append

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo)]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [ (1, return Nil)
              , (1, return (Cons x xs))]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq

instance Eq a => EqProp (List a) where (=-=) = eq

instance (Eq a, Eq e) => EqProp (Validation e a) where (=-=) = eq

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return $ Success x)
              , (1, return $ Failure y)]

instance Arbitrary a => Arbitrary (Product a) where
  arbitrary = arbitrary >>= return . Product

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = arbitrary >>= return . Sum

-- Exercises

data Pair a =
  Pair a a
  deriving (Eq, Show)

data Two a b =
  Two a b
  deriving (Eq, Show)

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return $ Pair x x

instance Eq a => EqProp (Pair a) where (=-=) = eq

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two a g) <*> (Two b x) = Two (a `mappend` b) (g x)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

main :: IO ()
main = do

  putStrLn "\nFunctor:"
  quickBatch $ functor (undefined :: Pair (String, String, Integer))
  quickBatch $ functor (undefined :: Two Integer (String, String, Integer))

  putStrLn "\nApplicative:"
  quickBatch $ applicative [("b", "w", 1) :: (String, String, Int)]
  quickBatch $ applicative (Cons (("b", "w", 1) :: (String, String, Int)) Nil)
  quickBatch $ applicative (undefined :: List (String, String, Int))
  quickBatch $ applicative (undefined :: Either  (String, String, Int) (String, String, Int))
  quickBatch $ applicative (undefined :: Validation (Sum Integer) (String, String, Product Integer))
  quickBatch $ applicative (undefined :: Validation (Product Integer) (Integer, Char, Int))
  quickBatch $ applicative (undefined :: Pair (Integer, Char, Int))
  quickBatch $ applicative (undefined :: Two (Product Integer) (String, String, Integer))

  putStrLn "\nMonoid:"
  quickBatch (monoid Twoo)  -- Should Fail
  quickBatch (monoid (undefined :: Bull)) -- Should Fail
  quickBatch (monoid ((Cons 0 Nil) :: List Int))
