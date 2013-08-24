import qualified Data.Char as Char      (toUpper)
import           System.Environment     (getArgs)
import           System.IO              (hPutStrLn, stderr)

import           Utils                  (sleep, readMaybe)
import qualified Rpc                    (start, implement)

echo :: String -> IO String
echo = return

usageInfo :: String
usageInfo = "Expecting exactly one argument as the port to serve on"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [portString] -> do
      case readMaybe portString :: Maybe Int of
        Just port -> do
          let numberOfWorkers = 5
          Rpc.start port numberOfWorkers
            [ Rpc.implement "echo" echo ]
        Nothing -> hPutStrLn stderr usageInfo
    _ -> hPutStrLn stderr usageInfo
