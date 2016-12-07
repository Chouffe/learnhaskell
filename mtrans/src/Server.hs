{-# LANGUAGE OverloadedStrings #-}

module Server where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))
import Data.Monoid (mconcat)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)

param' :: Parsable a => Text -> ExceptT String ActionM a
param' k = ExceptT $
           rescue (Right <$> param k)
                   (const
                    (return
                     (Left $ "The key: "
                             ++ show k
                             ++ "was missing")))


-- rescue :: ActionM a -> (Text -> ActionM a) -> ActionM a
-- param :: Parsable a => Text -> ActionM a

type Reco = (Integer, Integer, Integer, Integer)

tshow :: Show a => a -> Text
tshow = TL.pack . show

main = scotty 3000 $ do
  get "/" $ do
    reco <- runExceptT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      return ((a, b, c, d) :: Reco)
      -- return ((a, b, c, d) :: Reco)
    liftIO $ print reco
    case reco of
      (Left e) -> text (TL.pack e)
      (Right r) ->
        html $ mconcat ["<h1>Scotty, ", tshow r, " me up</h1>"]

liftReaderT :: Monad m => m a -> ReaderT r m a
liftReaderT m = ReaderT $ \r -> m

liftStateT :: Monad m => m a -> StateT s m a
liftStateT m = StateT $ \s -> m >>= \x -> return (x, s)
