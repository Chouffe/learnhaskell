module Addition where

import Test.QuickCheck
  ( Arbitrary
  , Gen
  , property
  , arbitrary
  , frequency
  , elements
  , choose)
import Test.Hspec
  ( shouldBe
  , describe
  , hspec
  , it
  )


divideBy :: Integral a => a -> a -> (a, a)
divideBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

mult :: (Num a, Ord a) => a -> a -> a
mult 0 _ = 0
mult _ 0 = 0
mult 1 x = x
mult x 1 = x
mult x y
  | y > x = mult y x
  | otherwise = y + mult (x - 1) y

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $
      ((1 + 1) :: Integer) > (1 :: Integer) `shouldBe` True
    it "2 + 2 is equal to 4" $
      ((2 + 2) :: Integer) `shouldBe` (4 :: Integer)
    it "x + 1 is always greate than x" $
      property $ \x -> x + 1 > (x :: Integer)
  describe "Division" $ do
    it "15 divided by 3 is 5" $
      divideBy (15 :: Integer) (3 :: Integer) `shouldBe` ((5, 0) :: (Integer, Integer))
    it "22 divided by 5 is 4 remainder 2" $
      divideBy (22 :: Integer) (5 :: Integer) `shouldBe` ((4, 2) :: (Integer, Integer))
  describe "Multiplication" $ do
    it "3 mult 2 is 6" $
      mult (3 :: Integer) (2 :: Integer) `shouldBe` (6 :: Integer)
    it "3 mult 3 is 9" $
      mult (3 :: Integer) (3 :: Integer) `shouldBe` (9 :: Integer)
    it "1 mult 0 is 0" $
      mult (1 :: Integer) (0 :: Integer) `shouldBe` (0 :: Integer)

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]
