module DebugProgram where

import           Control.Monad             (forever)
import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)

x :: Integer
x = 1

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
  (handle, _) <- accept sock
  printAndKickBack handle
  close handle
  where printAndKickBack conn = do
          msg <- recv conn 1024
          print msg
          sendAll conn msg

server :: IO ()
server = withSocketsDo $ do
  (serverAddr:_) <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing
                   (Just "79")
  putStrLn "Initializing..."
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 1
  logAndEcho sock
  close sock

  -- sock <- listen 12345
  -- logAndEcho sock
