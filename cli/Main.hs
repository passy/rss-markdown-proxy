{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where

import qualified Data.Text.Lazy     as T
import           Lib                (fetchFeed, transformRSS)
import           System.Environment (getArgs)

import           Data.Version             (Version (), showVersion)
import           Options.Applicative
import           Paths_rss_markdown_proxy (version)

main :: IO ()
main = do
  [url] <- getArgs
  rss <- T.unpack <$> fetchFeed url
  transformRSS rss >>= putStrLn
