{-# LANGUAGE InstanceSigs #-}

module Lib where

import Control.Monad

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.IO.Class

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  (Compose f) <*> (Compose a) = undefined -- TODO

newtype IdentityT m a =
  IdentityT { runIdentityT :: m a }
  deriving (Eq, Show)

instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT x) = IdentityT (fmap f x)

instance Applicative f => Applicative (IdentityT f) where
  pure = IdentityT . pure
  (IdentityT f) <*> (IdentityT x) = IdentityT (f <*> x)

instance Monad m => Monad (IdentityT m) where
  return = IdentityT . return
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= (runIdentityT . f)

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor f => Functor (MaybeT f) where
  fmap :: (a -> b) -> MaybeT f a -> MaybeT f b
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative f => Applicative (MaybeT f) where
  pure = MaybeT . pure . pure

  (<*>) :: MaybeT f (a -> b) -> MaybeT f a -> MaybeT f b
  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT ((<*>) <$> fab <*> mma)

innerMost :: [Maybe (Identity (a -> b))]
          -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

innerMost' :: [Maybe (Maybe (Identity (a -> b)))]
           -> [Maybe (Maybe (Identity a -> Identity b))]
innerMost' = (fmap . fmap . fmap) (<*>)

instance Monad m => (Monad (MaybeT m)) where
  return = MaybeT . return . return
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor f => Functor (EitherT e f) where
  fmap f (EitherT x) = EitherT $ (fmap . fmap) f x

instance Applicative f => Applicative (EitherT e f) where
  pure = EitherT . pure . pure
  (EitherT fab) <*> (EitherT mma) =
    EitherT ((<*>) <$> fab <*> mma)


instance Monad m => Monad (EitherT e m) where
  return = EitherT . return . return
  (EitherT ma) >>= f =
    EitherT $ do
      v <- ma
      case v of
        Left e -> return $ Left e
        Right x -> runEitherT $ f x


swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right y) = Left y

swapEitherT :: Functor f => EitherT e f a -> EitherT a f e
swapEitherT (EitherT ma) = EitherT (fmap swapEither ma)

-- either :: (a -> c) -> (b -> c) -> Either a b -> c
eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c

eitherT f g (EitherT ma) = do
  v <- ma
  case v of
    Left x  -> f x
    Right x -> g x


newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor f => Functor (ReaderT r f) where
  fmap f (ReaderT ma) = ReaderT $ (fmap . fmap) f ma

instance Applicative f => Applicative (ReaderT r f) where
  pure = ReaderT . pure . pure
  (ReaderT fab) <*> (ReaderT mma) =
    ReaderT ((<*>) <$> fab <*> mma)

instance Monad m => Monad (ReaderT r m) where
  return = ReaderT . return . return
  (ReaderT ma) >>= f = ReaderT $ \r -> ma r >>= \v -> runReaderT (f v) r

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor f => Functor (StateT s f) where
  fmap f (StateT sfa) = StateT $ \st -> let fa = sfa st -- :: f (a, s)
                                            g = (fmap (\(a, s) -> (f a, s)) fa)
                                         in g

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) = StateT $ \st -> smab st >>= \(f, s1) -> sma s1 >>= \(x, s2) -> return $ (f x, s2)

instance Monad m => Monad (StateT s m) where
  return x = StateT $ \s -> return (x, s)

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \st -> sma st >>= \(a, s1) -> runStateT (f a) s1

embedded :: M.MaybeT (E.ExceptT String (R.ReaderT () IO)) Int
embedded = return 1

maybeUnwrapped :: (E.ExceptT String (R.ReaderT () IO)) (Maybe Int)
maybeUnwrapped = M.runMaybeT embedded

eitherUnwrapped :: R.ReaderT () IO (Either String (Maybe Int))
eitherUnwrapped = E.runExceptT maybeUnwrapped

readerUnwrapped :: () -> IO (Either String (Maybe Int))
readerUnwrapped = R.runReaderT eitherUnwrapped

embedded' :: M.MaybeT (E.ExceptT String (R.ReaderT () IO)) Int
embedded' = M.MaybeT $ E.ExceptT $ R.ReaderT readerUnwrapped

-- MonadTransform

instance MonadTrans MaybeT where
  lift m = MaybeT $ liftM Just m

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> m >>= \x -> return (x, s)

-- MonadIO

instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO io = IdentityT $ liftIO io

instance (MonadIO m) => MonadIO (EitherT e m) where
  -- liftIO io = EitherT $ liftM Right $ liftIO io
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

-- Chaper Exercises

rDec :: Num a => ReaderT a Identity a
rDec = ReaderT $ Identity . ((-) 1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ (Identity . show)

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  putStrLn ("Hi: " ++ show r)
  return (r + 1)

sPrintIntAccum :: (Num a, Show a) => StateT a IO String
sPrintIntAccum = StateT $ \s -> do
  putStrLn ("Hi: " ++ show s)
  return $ (show s, s + 1)
