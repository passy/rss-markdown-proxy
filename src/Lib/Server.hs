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
import           Data.TCache             (atomically)
import           Data.TCache.Memoization (cachedByKeySTM)

import           Data.Default            (Default (), def)
import           Lib                     (fetchFeed, transformRSS)

type Seconds = Int

-- | Cache expiration time in seconds.
cacheTime :: Seconds
cacheTime = 60 * 5

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
  in atomically $ cachedByKeySTM url' cacheTime perform

server :: ServerOptions -> IO ()
server opts =
  S.scotty (unPort $ opts ^. port) $
    S.get "/feed.rss" $ do
      res <- liftIO $ transformUrlCached $ opts ^. url
      S.text $ T.pack res
