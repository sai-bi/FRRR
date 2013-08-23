module String
  (strip, leftSplit2) where

import qualified Data.List as List             (intersperse)
import qualified Data.List.Split as List.Split (splitOn)
import qualified Data.Text as T                (strip, pack, unpack)

strip :: String -> String
strip = T.unpack . T.strip . T.pack

leftSplit2 :: Char -> String -> Maybe (String, String)
leftSplit2 on s =
  case List.Split.splitOn [on] s of
       []    -> Nothing
       [_]   -> Nothing
       hd:tl -> Just (hd, concat (List.intersperse [on] tl))
