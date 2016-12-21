module MyExceptions where

import           Control.Exception

data NotDivThree = NotDivThree Int deriving (Eq, Show)

instance Exception NotDivThree

data NotEven = NotEven Int deriving (Eq, Show)

instance Exception NotEven


-- Exception instances are derivable. No need to write my own instances.

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO (NotDivThree i)
  | odd i = throwIO (NotEven i)
  | otherwise = return i

catchNotDivThree :: IO Int -> (NotDivThree -> IO Int) -> IO Int
catchNotDivThree = catch

catchNotEven :: IO Int -> (NotEven -> IO Int) -> IO Int
catchNotEven = catch

-- catches :: IO a -> [Handler a] -> IO a

catchNotEven2 :: IO Int -> IO Int
catchNotEven2 ioInt =
    catches ioInt
    [ Handler (\(NotEven _) -> return maxBound)
    , Handler (\(NotDivThree _) -> return minBound)]
