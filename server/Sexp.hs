{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Sexp
( Sexp (..), ToSexp (..)
, printInSexp, hPrintInSexp
)
where

import Data.List (intersperse)
import System.IO (Handle, hPutStrLn)

data Sexp = Atom String | List [Sexp]

instance Show Sexp where
  show (Atom s)     = s
  show (List sexps) = "(" ++ concat (intersperse " " (map show sexps)) ++ ")"

class ToSexp a where
  toSexp :: a -> Sexp

instance ToSexp Sexp where
  toSexp = id

instance ToSexp () where
  toSexp = Atom . show

instance ToSexp Int where
  toSexp = Atom . show

instance ToSexp Double where
  toSexp = Atom . show

instance (ToSexp a) => ToSexp (Maybe a) where
  toSexp (Just a) = List [toSexp a]
  toSexp Nothing  = List []

instance (ToSexp a) => ToSexp [a] where
  toSexp a = List (map toSexp a)

instance ToSexp [Char] where
  toSexp a
    | not (any (`elem` " ") a) = Atom a
    | otherwise                = (Atom . show) a

hPrintInSexp :: (ToSexp a) => Handle -> a -> IO ()
hPrintInSexp h = hPutStrLn h . show . toSexp

printInSexp :: (ToSexp a) => a -> IO ()
printInSexp = putStrLn . show . toSexp
