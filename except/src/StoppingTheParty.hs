module StoppingTheParty where

import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Control.Monad      (forever)
import           Data.Typeable      (typeOf)
import           System.Random      (randomRIO)

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Integer)
  if i `elem` [1..9]
    then throwIO DivideByZero
    else throwIO StackOverflow

tryS :: IO () -> IO (Either ArithException ())
tryS = try

main :: IO ()
main = forever $ do
  _ <- tryS randomException
  putStrLn "Live to loop another day!"
  -- microseconds
  threadDelay (1 * 1000000)

throwIOPlayground :: IO ()
throwIOPlayground = do
  throwIO DivideByZero `catch` handler
  putStrLn "blah"
    where handler :: ArithException -> IO ()
          handler e = print (typeOf e) >> putStrLn "Failure"

