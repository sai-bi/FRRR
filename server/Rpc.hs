{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Rpc
( start
) where

import           Prelude                hiding (catch)
import           Control.Concurrent     (forkIO)
import           Network
import           System.IO
import           System.Posix           (installHandler, sigPIPE, Handler (Ignore))

response :: String
response =
  "HTTP/1.1 200 OK\n\
  \Content-Length: 22\n\
  \Connection: keep-alive\n\
  \Content-Type: text/html\n\
  \Access-Control-Allow-Origin: *\n\
  \\n\
  \{ \"number\": 68446400 }\n"

echo :: String -> Handle -> IO ()
echo clientName handle = do
  atEOF <- hIsEOF handle
  if atEOF
    then return ()
    else do
      msg <- hGetLine handle
      putStrLn $ clientName ++ ":\n" ++ show msg ++ "\n"
      hPutStrLn handle response
      echo clientName handle

acceptLoop :: Socket -> IO ()
acceptLoop listenSock = do
  (handle, host, port) <- accept listenSock
  hSetBuffering handle NoBuffering
  let clientName = show host ++ " @ " ++ show port
  putStrLn $ "Accepted " ++ clientName ++ "\n"
  _ <- forkIO $ echo clientName handle
  acceptLoop listenSock

-- | Start an RPC server.
start :: Int -> IO ()
start port = withSocketsDo $ do
  _ <- installHandler sigPIPE Ignore Nothing
  hSetBuffering stdout LineBuffering
  listenSock  <- listenOn $ PortNumber (fromIntegral port)
  acceptLoop listenSock
