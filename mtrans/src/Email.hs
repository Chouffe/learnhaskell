{-# LANGUAGE OverloadedStrings #-}

module Email where

import           Control.Applicative
import qualified Data.Map            as Map
import           Data.Text
import qualified Data.Text.IO        as T

data LoginError =
    InvalidEmail
  | NoSuchUser
  | WrongPassword
  deriving (Show, Eq)

users :: Map.Map Text Text
users = Map.fromList [ ("example.com", "qwerty123")
                     , ("localhost", "password")
                     ]

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [_, domain] -> Right domain
    _           -> Left InvalidEmail

printResult' :: Either LoginError Text -> IO ()
printResult' domain =
  putStrLn $ either (const "Error: Invalid domain")
                    (\text -> "Domain: " ++ unpack text) domain
  -- case domain of
  --   Right text        -> putStrLn $ "Domain: " ++ unpack text
  --   Left InvalidEmail -> putStrLn "Error: Invalid domain"

getToken :: IO (Either LoginError Text)
getToken = do
  putStrLn "Enter email address:"
  email <- pack <$> getLine
  return (getDomain email)

-- Horrible Haskell Function that mixes Either and IO monads
-- Use MonadTransformers to compose them
userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken

  case token of
    Right domain ->
      case Map.lookup domain users of
        Nothing           -> return (Left NoSuchUser)
        Just userPassword -> do
          putStrLn "Enter password:"
          password <- fmap pack getLine
          if userPassword == password
          then return token
          else return (Left WrongPassword)
    left -> return left

-- Writing my own EitherIO monad
data EitherIO e a = EitherIO { runEitherIO :: IO (Either e a) }

instance Functor (EitherIO e) where
  -- Silly way to write it
  -- fmap f (EitherIO action) = EitherIO $ do
  --   eit <- action
  --   return $ fmap f eit
  fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
  pure = EitherIO . return . Right

  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return = EitherIO . return . Right


  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)

getToken' :: EitherIO LoginError Text
getToken' = do
  -- Even more horrible because we need lift
  EitherIO (fmap Right (T.putStrLn "Enter email address:"))
  input <- EitherIO $ fmap Right T.getLine
  EitherIO $ return (getDomain input)


liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO (fmap Right x)

liftEither :: Either e a -> EitherIO e a
liftEither = EitherIO . return

-- Run it with runEitherIO getToken''
getToken'' :: EitherIO LoginError Text
getToken'' = do
  liftIO (T.putStrLn "Enter email address:")
  input <- liftIO T.getLine
  liftEither (getDomain input)

userLogin'' :: EitherIO LoginError Text
userLogin'' = do
  token <- getToken''
  userpw <- maybe (liftEither (Left NoSuchUser))
                   return
                  (Map.lookup token users)
  password <- liftIO (putStrLn "Enter your password: " >> T.getLine)

  if userpw == password
  then return token
  else liftEither (Left WrongPassword)


getResult :: Either LoginError Text -> String
getResult res =
  case res of
    Right token        -> "Logged in with token: " ++ unpack token
    Left InvalidEmail  -> "Invalid email address entered"
    Left NoSuchUser    -> "No user with that email exists"
    Left WrongPassword -> "Wrong password"

printResult'' :: Either LoginError Text -> IO ()
printResult'' = putStrLn . getResult

-- runEitherIO userLogin'' >>= printResult''

-- Exception handling with EitherIO
throwE :: e -> EitherIO e a
throwE x = liftEither (Left x)

catchE :: EitherIO e a -> (e -> EitherIO e a) -> EitherIO e a
catchE throwing handler =
  EitherIO $ do
    result <- runEitherIO throwing
    case result of
      Left failure -> runEitherIO $ handler failure
      success      -> return success

wrongPasswordHandler :: LoginError -> EitherIO LoginError Text
wrongPasswordHandler WrongPassword = do
  liftIO (putStrLn "Wrong Password, one more chance.")
  userLogin''
wrongPasswordHandler err = throwE err

printError :: LoginError -> EitherIO LoginError a
printError err = do
  liftIO . putStrLn $ case err of
                        WrongPassword -> "Wrong password"
                        NoSuchUser    -> "No such users"
                        InvalidEmail  -> "Invalid email adress entered"
  throwE err

loginDialogue :: EitherIO LoginError ()
loginDialogue = do
  let retry = userLogin'' `catchE` wrongPasswordHandler
  token <- retry `catchE` printError
  liftIO $ putStrLn $ "Logged in with token: " ++ unpack token
