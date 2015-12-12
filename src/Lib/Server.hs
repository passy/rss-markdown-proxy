{-# LANGUAGE OverloadedStrings #-}

module Lib.Server
    ( server
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

import           Lib                           (fetchFeed, transformRSS)
import           Control.Lens                  hiding (deep)
import           Text.XML.HXT.Core

type Seconds = Int

-- | Cache expiration time in seconds.
cacheTime :: Seconds
cacheTime = 60 * 5

-- TODO: Obviously, this should be dynamic at least to some degree.
feedUrl :: String
feedUrl = "http://feeds.soundcloud.com/users/soundcloud:users:189413584/sounds.rss"

transformUrlCached :: String -> IO String
transformUrlCached url =
  let perform = transformRSS =<< T.unpack <$> fetchFeed feedUrl
  in atomically $ cachedByKeySTM url cacheTime perform

server :: IO ()
server =
  S.scotty 3000 $
    S.get "/feed.rss" $ do
      res <- liftIO $ transformUrlCached feedUrl
      S.text $ T.pack res
