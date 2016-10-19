import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap g (fmap f x)) == (fmap (g . f) x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
  putStrLn "\nRunning QuickCheck:"
  quickCheck $ \x -> functorIdentity (x :: Maybe Int)
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck $ \x -> functorIdentity (x :: Either String Int)
  quickCheck $ \x -> functorCompose (*2) (+1) (x :: Either String Int)
  quickCheck (functorCompose' :: IntFC)
