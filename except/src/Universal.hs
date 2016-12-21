{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Universal where

import           Control.Exception  (ArithException (..), AsyncException (..),
                                     Exception, SomeException, catch, throwIO,
                                     try)
import           Control.Monad      (mapM)
import           Data.Typeable
import           System.Environment (getArgs)

data MyException = forall e . (Show e, Typeable e) => MyException e

instance Show MyException where
    showsPrec p (MyException e) =
        showsPrec p e

multiError :: Int -> Either MyException Int
multiError n =
  case n of
    0 -> Left (MyException DivideByZero)
    1 -> Left (MyException StackOverflow)
    _ -> Right n

data SomeError = Arith ArithException
               | Async AsyncException
               | SomethingElse
               deriving (Show)

discriminateError :: MyException -> SomeError
discriminateError (MyException e) =
    case cast e of
      (Just arith) -> Arith arith
      Nothing ->
          case cast e of
            (Just async) -> Async async
            Nothing      -> SomethingElse

runDisc n =
    either discriminateError
    (const SomethingElse) (multiError n)

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

willIFail2 :: Integer -> IO (Either ArithException ())
willIFail2 denom = try $ print $ div 5 denom

onlyReport :: (Show e, Typeable e) => IO (Either e a) -> IO ()
onlyReport action = do
  result <- action
  case result of
    Right r -> putStrLn "Success"
    Left e  -> putStrLn ("Error: " ++ show e) >> print (typeOf e)

willIFail' :: Integer -> IO ()
willIFail' denom = print (div 5 denom) `catch` handler
  where handler :: ArithException -> IO ()
        handler = print

mmain :: IO ()
mmain = do
  line <- getLine
  mapM_ (onlyReport . willIFail . read) (words line)

canICatcth :: Exception e => e -> IO (Either SomeException ())
canICatcth = try . throwIO
