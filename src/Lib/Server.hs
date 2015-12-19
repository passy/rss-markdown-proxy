{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Server
    ( server
    , ServerOptions(..)
    , Port(..)
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

-- | Cache expiration time in seconds.
cacheTime :: Seconds
cacheTime = 60

newtype Port = Port Int
  deriving (Read, Show)

unPort :: Port -> Int
unPort (Port i) = i

-- | Command line options provided to start up the server.
data ServerOptions = ServerOptions
  { _url  :: String
  , _port :: Port
  }

makeLenses ''ServerOptions

instance Default Port where
  def = Port 3000

transformUrlCached :: String -> IO String
transformUrlCached url' =
  let perform = transformRSS =<< T.unpack <$> fetchFeed url'
      seconds = fromEnum $ toSeconds cacheTime
  in atomically $ cachedByKeySTM url' seconds perform

server :: ServerOptions -> IO ()
server opts =
  S.scotty (unPort $ opts ^. port) $
    S.get "/feed.rss" $ do
      res <- liftIO $ transformUrlCached $ opts ^. url
      S.setHeader "Content-Type" "application/rss+xml;charset=utf-8"
      S.text $ T.pack res
