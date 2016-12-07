module Fix where

import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.IO.Class

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
  v <- getLine
  case isValid v of
    True -> return $ Just v
    False -> return Nothing

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  print excite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
