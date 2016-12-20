{-# LANGUAGE OverloadedStrings #-}

module Lib.CLI
  ( cliMain
  ) where

import qualified Data.Text.Lazy as T
import Lib (fetchFeed, transformRSS)
import System.Environment (getArgs)
cliMain :: IO ()
cliMain = do
  [url] <- getArgs
  rss <- T.unpack <$> fetchFeed url
  transformRSS rss >>= putStrLn
