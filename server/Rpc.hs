{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Rpc
( implement, start
) where

import           Prelude                hiding (catch)
import           Control.Exception      (catchJust)
import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM
import           Control.Monad          (forever)
import qualified Data.List as List      (foldl')
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           Network                (Socket, listenOn, accept, withSocketsDo, PortID (PortNumber))
import           System.IO
import           System.IO.Error
import           System.Posix           (installHandler, sigPIPE, Handler (Ignore))
import           System.Time            (ClockTime, getClockTime)

import qualified String
import           Sexp
import           Utils

data Response =
    Heartbeat
  | BadRequest
  | UnimplementedRpc
  | Ack Int
  | Working Int
  | Result (Int, String)

data Job= Job
  { jobHandle      :: Handle
  , jobToken       :: Int
  , jobHandler     :: String -> IO String
  , jobRequestBody :: String
  , jobOutQueue    :: TChan Response
  }

instance ToSexp Response where
  toSexp Heartbeat        = Atom "Heartbeat"
  toSexp BadRequest       = Atom "Bad_request"
  toSexp UnimplementedRpc = Atom "Unimplemented_rpc"
  toSexp (Ack token)      = List [ Atom "Ack"
                                 , List [Atom "token", toSexp token]
                                 ]
  toSexp (Working token)  = List [ Atom "Working"
                                 , List [Atom "token", toSexp token]
                                 ]
  toSexp (Result (token, responseBody)) =
    List [ Atom "Result"
         , List [Atom "token" , toSexp token]
         , List [Atom "response_body", toSexp responseBody]
         ]

newtype Implementations = Implementations
  { getImplementations :: Map.Map String (String -> IO String) }

implement :: (Read a, ToSexp b) =>
  String -> (a -> IO b) -> (String, String -> IO String)
implement rpcName f = (rpcName, fmap (show . toSexp) . f . read)

createImplementations ::
  [(String, String -> IO String)] -> Either [String] Implementations
createImplementations impl's =
  let (_, dups) = List.foldl' -- strict left fold
                    (\(names', dups') (n, _) ->
                      if Set.member n names'
                        then (names', Set.insert n dups')
                        else (Set.insert n names', dups'))
                    (Set.empty, Set.empty)
                    impl's
  in
  if Set.null dups
    then Right $ Implementations (Map.fromList impl's)
    else Left  $ Set.toList dups

multiReadTChan :: TChan a -> STM [a]
multiReadTChan = loop []
  where
  loop acc ch = do
    empty <- isEmptyTChan ch
    if empty
      then return acc
      else do
        a <- readTChan ch
        loop (a:acc) ch

multiWriteTChan :: TChan a -> [a] -> STM ()
multiWriteTChan _ [] = return ()
multiWriteTChan ch (x:xs) = do
  writeTChan ch x
  multiWriteTChan ch xs

-- Cancel jobs from client matching handle
canceljob :: TChan Job-> Handle -> IO ()
canceljob jobQueue handle = do
  (oldCount, newCount) <- atomically $ do
    oldjob <- multiReadTChan jobQueue
    let newjob  = filter (\job -> jobHandle job /= handle) oldjob
    multiWriteTChan jobQueue newjob
    let oldCount = length oldjob
    let newCount = length newjob
    return (oldCount, newCount)
  putStrLn $ "Cancelled " ++ show (oldCount - newCount) ++ " jobs from queue"
  putStrLn $ show newCount ++ " remaining"

worker :: TChan Job-> IO ()
worker jobQueue = do
  job <- atomically $ readTChan jobQueue
  atomically $ writeTChan (jobOutQueue job) $ Working (jobToken job)
  -- Make sure the job is done right in the worker thread
  ! responseBody <- (jobHandler job) (jobRequestBody job)
  atomically $ writeTChan (jobOutQueue job) $ Result (jobToken job, responseBody)
  worker jobQueue

-- Toekn bug
-- Token might overflow
reader :: Implementations -> TChan Job-> TChan Response -> Handle -> IO ()
reader implementations jobQueue outQueue handle =
  readerLoop 1
  where
    readerLoop token = do
      atEOF <- hIsEOF handle
      if atEOF
        -- Client has no more requests but is still interested in the results of
        -- open requests (those in the job queue).
        then return ()
        else do
          message <- fmap (filter (/= '\r')) $ hGetLine handle
          case (String.leftSplit2 ' ' message) of
            Nothing -> do
              atomically $ writeTChan outQueue BadRequest
              readerLoop token
            Just (rpcName, requestBody) -> do
              case Map.lookup rpcName (getImplementations implementations) of
                Nothing -> do
                  atomically $ writeTChan outQueue UnimplementedRpc
                  readerLoop token
                Just handler -> do
                  atomically $ writeTChan outQueue $ Ack token
                  atomically $ writeTChan jobQueue $ Job { jobHandle = handle
                                                         , jobToken = token
                                                         , jobHandler = handler
                                                         , jobRequestBody = requestBody
                                                         , jobOutQueue = outQueue
                                                         }
                  readerLoop (token + 1)

-- Special case:
--      The corresponding reader has finished and;
--      there are no more open requests
--      (so that there are no more responses to send)
-- Action:
--      Close the writer

writer :: TChan Job-> TChan Response -> Handle -> IO ()
writer jobQueue outQueue handle = do
  response <- atomically $ readTChan outQueue
  -- We want to catch the case in which the reading end is closed.
  -- Otherwise on a POSIX system, SIGPIPE will by default terminate the program.
  catchJust
    (\exn ->
      -- We'd like to catch just the exception raised by SIGPIPE, namely the
      -- 'ResourceVanished' data constructor, against which ideally we should
      -- pattern-match. But unluckily the 'System.IO.Error' module does not
      -- export it. So we have to resort to this hack.
      --
      -- Issue: Will miscatch remote side reading end close
      if show (ioeGetErrorType exn) == "resource vanished"
        then Just exn
        else Nothing)
    (do hPrintInSexp handle response
        writer jobQueue outQueue handle)
    (\_exn -> do
      -- A closed reading end means that the client is no longer waiting for the
      -- results of its requests. So we should cancel all the client's jobs to
      -- free up worker capacity.
      hPutStrLn stderr $ "Reading end closed, canceling jobs of " ++ show handle
      canceljob jobQueue handle)
      -- There are at least two ways to cancel jobs. One is physically removing
      -- cancelled jobs from the job queue, while the other is just letting
      -- workers to ignore them.
      -- We prefer the first approach because it doesn't add worker's burden and
      -- outQueue can then be GC'ed.

serveClient :: Implementations -> TChan Job-> Handle -> IO ()
serveClient implementations jobQueue handle = do
  outQueue <- atomically $ newTChan
  _ <- forkIO $ reader implementations jobQueue outQueue handle
  _ <- forkIO $ writer jobQueue outQueue handle
  return ()

response :: String
response =
  "HTTP/1.1 200 OK\n\
  \Content-Length: 22\n\
  \Connection: keep-alive\n\
  \Content-Type: text/html\n\
  \Access-Control-Allow-Origin: *\n\
  \\n\
  \{ \"number\": 68446400 }"

echo handle host port = do
  msg <- hGetLine handle
  putStrLn $ "Received from " ++ show host ++ " via port " ++ show port ++ ":\n" ++ show msg ++ "\n"
  hPutStrLn handle response
  echo handle host port

acceptLoop :: Implementations -> Socket -> TChan Job-> IO ()
acceptLoop implementations sock jobQueue =
  forever $ do
    (handle, host, port) <- accept sock
    hSetBuffering handle NoBuffering
    putStrLn $ "Accepted " ++ show host ++ " on port " ++ show port ++ "\n"
    --_ <- forkIO $ serveClient implementations jobQueue handle
    _ <- forkIO $ echo handle host port
    return ()

-- | Start an RPC server.
start :: Int -> Int -> [(String, String -> IO String)] -> IO ()
start port numberOfWorkers impl's = withSocketsDo $ do
  _ <- installHandler sigPIPE Ignore Nothing
  hSetBuffering stdout LineBuffering
  case createImplementations impl's of
    Left dups -> do
      hPutStrLn stderr $ "Duplicate RPC names: " ++ show dups
    Right implementations -> do
      listenSock  <- listenOn $ PortNumber (fromIntegral port)
      jobQueue    <- atomically $ newTChan
      _ <- forkNIO numberOfWorkers $ worker jobQueue
      acceptLoop implementations listenSock jobQueue
