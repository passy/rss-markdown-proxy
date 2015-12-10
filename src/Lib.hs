{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( server
    ) where

import qualified Web.Scotty as S
import qualified Network.Wreq as W
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.ByteString.Lazy as BS

import Control.Monad.IO.Class (liftIO)
import Control.Lens

feedUrl :: String
feedUrl = "http://feeds.soundcloud.com/users/soundcloud:users:189413584/sounds.rss"

server :: IO ()
server = do
  S.scotty 3000 $ do
    S.get "/feed.rss" $ do
      res <- liftIO $ fetchFeed feedUrl
      S.text res

fetchFeed :: String -> IO T.Text
fetchFeed url = do
  r <- W.get url
  return $ TE.decodeUtf8 $ r ^. W.responseBody
