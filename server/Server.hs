import           System.Environment     (getArgs)
import           System.IO              (hPutStrLn, stderr)

import           Utils                  (readMaybe)
import qualified Rpc

usageInfo :: String
usageInfo = "Expecting exactly one argument as the port to serve on"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [portString] -> do
      case readMaybe portString :: Maybe Int of
        Just port -> Rpc.start port
        Nothing -> hPutStrLn stderr usageInfo
    _ -> hPutStrLn stderr usageInfo
