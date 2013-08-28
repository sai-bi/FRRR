{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import           System.Process (readProcess)

import           Snap.Core
import           Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [("detect/:url", detect)]

detect :: Snap ()
detect = do
  maybeUrl <- getParam "url"
  case maybeUrl of
    Nothing  -> writeBS ""
    Just url -> do
      faces <- liftIO $ readProcess "../detect/src/detect.py" [] (B.unpack url)
      writeBS $ B.pack faces
