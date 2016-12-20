{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Server
  ( server
  ) where

import qualified Data.Text.Lazy as T
import qualified Web.Scotty as S

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Hourglass.Types (Seconds(), toSeconds)
import Data.TCache (atomically)
import Data.TCache.Memoization (cachedByKeySTM)

import Lib (fetchFeed, transformRSS)
import Lib.Types (ServerOptions(), port, unPort, url, metricsPort)
import qualified Lib.Metrics as Metrics

-- | Cache expiration time in seconds.
cacheTime :: Seconds
cacheTime = 60

transformUrlCached :: String -> IO String
transformUrlCached url' =
  let perform = transformRSS =<< T.unpack <$> fetchFeed url'
      seconds = fromEnum $ toSeconds cacheTime
  in atomically $ cachedByKeySTM url' seconds perform

server :: ServerOptions -> IO ()
server opts = do
  metricsMiddleware <- Metrics.getMetricsMiddleware (opts ^. metricsPort)

  S.scotty (opts ^. port & unPort) $ do
    S.middleware metricsMiddleware
    S.get "/feed.rss" $ do
      res <- liftIO . transformUrlCached $ opts ^. url
      S.setHeader "Content-Type" "application/rss+xml;charset=utf-8"
      S.text $ T.pack res
