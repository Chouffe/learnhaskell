{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

data Config = Config {
    -- that's one, one click!
    -- - two...two clicks!
    -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

-- Stuff inside ScottyT is, except for things that escape
-- via IO, effectively read-only so we can't use StateT.
-- It would overcomplicate things to attempt to do so and
-- you should be using a proper database for production
-- applications.

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  case M.lookup k m of
    Nothing -> (M.insert k 1 m, 1)
    Just x -> (M.insert k (x + 1) m, (x + 1))

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    liftIO $ print (unprefixed :: Text)
    config <- lift $ ask
    mcounts <- lift $ asks counts
    mprefix <- lift $ asks prefix
    liftIO $ print (prefix config)
    -- let key' = mappend (prefix config) unprefixed
    let key' = mappend mprefix unprefixed
    (newMap, newInteger) <- liftIO $ bumpBoomp key' <$> readIORef (counts config)
    liftIO $ writeIORef (counts config) newMap
    liftIO $ print newMap
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config {counts=counter, prefix=(TL.pack prefixArg)}
      runR = flip runReaderT config
  scottyT 3000 runR app
