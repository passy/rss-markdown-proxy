module Lib.Metrics
  ( getMetricsMiddleware
  ) where

import           Lib.Types   (Metrics, Port)
import           Network.Wai (Middleware)

getMetricsMiddleware :: Port Metrics -> IO Middleware
getMetricsMiddleware = const $ return id
