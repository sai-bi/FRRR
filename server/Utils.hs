module Utils
  ( readMaybe
  , interleave, lookupExn
  , sleep
  , fetchPage
  , forkNIO
  ) where

import Prelude hiding (catch)

import Control.Exception (catchJust)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad (replicateM)
import qualified Data.ByteString.Lazy.UTF8 as B (toString)
import Data.List (transpose)
import Network.HTTP.Conduit (simpleHttp, HttpException (..))
import System.IO (hPutStrLn, stderr)

readMaybe :: Read a => String -> Maybe a
readMaybe s =
  case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

interleave :: [[a]] -> [a]
interleave = concat . transpose

lookupExn :: String -> [(String, String)] -> String
lookupExn key alist =
  case lookup key alist of
       Just value -> value
       Nothing    -> error $ "lookupExn: failed to find the key " ++ show key ++
                             " in the association list " ++ show alist

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000000)

fetchPage :: String -> IO String
fetchPage url =
  catchJust
    (\exn ->
      case exn of
        FailedConnectionException _host _port -> Just exn
        ResponseTimeout -> Just exn
        InternalIOException _ -> Just exn
        _ -> Nothing)
    (fmap B.toString (simpleHttp url))
    (\exn -> do
      hPutStrLn stderr $ "fetchPage: Caught " ++ show exn ++ ", retrying"
      fetchPage url)

forkNIO :: Int -> IO () -> IO [ThreadId]
forkNIO n action = replicateM n $ forkIO action
