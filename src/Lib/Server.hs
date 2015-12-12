{-# LANGUAGE OverloadedStrings #-}

module Lib.Server
    ( server
    , ServerOptions(..)
    , Port(..)
    ) where

import qualified Data.ByteString.Lazy          as BS

import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Encoding       as TE
import qualified Network.Wreq                  as W
import qualified Web.Scotty                    as S

import           Control.Monad.IO.Class        (liftIO)
import           Data.TCache                   (atomically)
import           Data.TCache.Memoization       (cachedByKeySTM)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Markdown                 (def, markdown)

import           Control.Lens                  hiding (deep)
import           Data.Default                  (Default (), def)
import           Lib                           (fetchFeed, transformRSS)
import           Text.XML.HXT.Core

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
  { url  :: String
  , port :: Port
  }

instance Default Port where
  def = Port 3000

transformUrlCached :: String -> IO String
transformUrlCached url =
  let perform = transformRSS =<< T.unpack <$> fetchFeed url
  in atomically $ cachedByKeySTM url cacheTime perform

server :: ServerOptions -> IO ()
server opts =
  S.scotty (unPort $ port opts) $
    S.get "/feed.rss" $ do
      res <- liftIO $ transformUrlCached $ url opts
      S.text $ T.pack res
