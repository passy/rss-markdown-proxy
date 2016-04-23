{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE CPP               #-}

module Lib.Server
    ( server
    , ServerOptions(..)
    , Port(..)
    , Server()
    , Metrics()
    ) where


import qualified Data.Text.Lazy          as T
import qualified Web.Scotty              as S

import           Control.Lens
import           Control.Monad.IO.Class  (liftIO)
import           Data.Default            (Default (), def)
import           Data.Hourglass.Types    (Seconds (), toSeconds)
import           Data.TCache             (atomically)
import           Data.TCache.Memoization (cachedByKeySTM)

import           Lib                     (fetchFeed, transformRSS)

-- import System.Remote.Monitoring (serverMetricStore, forkServer)

-- | Cache expiration time in seconds.
cacheTime :: Seconds
cacheTime = 60

newtype Port a = Port Int
  deriving (Read, Show)

unPort :: Port a -> Int
unPort (Port i) = i

data Server
data Metrics

-- | Command line options provided to start up the server.
data ServerOptions = ServerOptions
  { _url  :: String
  , _port :: Port Server
  , _metricsPort :: Port Metrics
  }

makeLenses ''ServerOptions

instance Default (Port Server) where
  def = Port 3000

instance Default (Port Metrics) where
  def = Port 3001

transformUrlCached :: String -> IO String
transformUrlCached url' =
  let perform = transformRSS =<< T.unpack <$> fetchFeed url'
      seconds = fromEnum $ toSeconds cacheTime
  in atomically $ cachedByKeySTM url' seconds perform

server :: ServerOptions -> IO ()
server opts =
  S.scotty (opts ^. port & unPort) .
    S.get "/feed.rss" $ do
      res <- liftIO . transformUrlCached $ opts ^. url
      S.setHeader "Content-Type" "application/rss+xml;charset=utf-8"
      S.text $ T.pack res
