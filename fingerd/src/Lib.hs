{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- {-# LANGUAGE RecordWildCards   #-}

module Lib where

import           Control.Exception
import           Control.Monad                (forever)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           Database.SQLite.Simple       hiding (bind, close)
import qualified Database.SQLite.Simple       as SQLite
import           Database.SQLite.Simple.Types
import           Network.Socket               hiding (close, recv)
import qualified Network.Socket               as Sock
import           Network.Socket.ByteString    (recv, sendAll)
import           Text.RawString.QQ

import           Data.List                    (intersperse)

data User =
  User { userId        :: Integer
       , username      :: Text
       , shell         :: Text
       , homeDirectory :: Text
       , realName      :: Text
       , phone         :: Text
       } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT,
   homeDirectory TEXT,
   realName TEXT,
   phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * FROM users"

getUserQuery :: Query
getUserQuery = "SELECT * FROM USERS where username = ?"

-- Exceptions

data DuplicateData = DuplicateData deriving (Eq, Show)

instance Exception DuplicateData

-- Type synonym for inserted users
type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn usrname = do
  result <- query conn getUserQuery (Only usrname)
  case result of
    []     -> return Nothing
    [user] -> return $ Just user
    _      -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
    where meRow :: UserRow
          meRow = (Null, "chouffe", "/bin/zsh", "/home/chouffe", "Arthur Caillau", "111-123-4567")

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) =
  BS.concat [ "Login: ", e username, "\t\t\t\t"
            , "Name: ", e realName, "\n"
            , "Directory: ", e homeDir, "\t\t\t\t"
            , "Shell: ", e shell, "\n"]
  where e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc usrname = do
  maybeUser <- getUser dbConn usrname
  case maybeUser of
    Nothing -> putStrLn ("Could not find matching user for username: " ++ show usrname)
    Just user -> sendAll soc (formatUser user)

removeTrailing :: Text -> Text
removeTrailing = T.filter (`notElem` ['\r', '\n'])

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name   -> returnUser dbConn soc (removeTrailing (decodeUtf8 name))

handlequeries :: Connection -> Socket -> IO ()
handlequeries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got Connection, handling query..."
  handleQuery dbConn soc
  Sock.close soc

server :: IO ()
server = withSocketsDo $ do
  (serverAddr:_) <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing
                   (Just "79")
  putStrLn "Initializing socket..."
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  putStrLn "...Binding socket..."
  Sock.bind sock (addrAddress serverAddr)
  putStrLn "...Listening..."
  listen sock 1
  putStrLn "Initializing DB connection..."
  conn <- open "finger.db"
  putStrLn "Listening for incoming requests on port 79"
  handlequeries conn sock
  putStrLn "Closing DB"
  SQLite.close conn
  putStrLn "Closing Socket"
  Sock.close sock
