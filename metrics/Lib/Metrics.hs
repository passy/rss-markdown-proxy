{-# LANGUAGE OverloadedStrings #-}

module Lib.Metrics where

import Lib.Types (Metrics, Port, unPort)
import Network.Wai (Middleware)
import System.Remote.Monitoring (forkServer, serverMetricStore)
import qualified Network.Wai.Metrics as Metrics

getMetricsMiddleware :: Port Metrics -> IO Middleware
getMetricsMiddleware port = do
  store <- serverMetricStore <$> forkServer "localhost" (unPort port)
  waiMetrics <- Metrics.registerWaiMetrics store
  return $ Metrics.metrics waiMetrics
