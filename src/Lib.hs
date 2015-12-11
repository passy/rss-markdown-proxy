{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( server
    , readDescriptions
    ) where

import qualified Data.ByteString.Lazy    as BS
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Network.Wreq            as W
import qualified Web.Scotty              as S

import           Control.Arrow           ((>>>))
import           Control.Monad.IO.Class  (liftIO)
import Debug.Trace

import           Control.Lens            hiding (deep)
import           Text.XML.HXT.Core

feedUrl :: String
feedUrl = "http://feeds.soundcloud.com/users/soundcloud:users:189413584/sounds.rss"


itunesNs :: String
itunesNs = "http://www.itunes.com/dtds/podcast-1.0.dtd"

type Description = String

server :: IO ()
server =
  S.scotty 3000 $
    S.get "/feed.rss" $ do
      res <- liftIO $ fetchFeed feedUrl
      let doc = readString [withWarnings yes] $ T.unpack res
      descs <- liftIO $ readDescriptions doc
      S.text $ T.pack $ show descs

fetchFeed :: String -> IO T.Text
fetchFeed url = do
  r <- W.get url
  return $ TE.decodeUtf8 $ r ^. W.responseBody

readDescriptions :: IOSLA (XIOState ()) XmlTree XmlTree -> IO [Description]
readDescriptions doc =
  runX $ doc >>> selectDescriptions //> getText

  where
    summaryName = traceShowId (mkNsName "summary" itunesNs)
    selectDescriptions = getChildren >>> isElem >>> hasQName summaryName

atTag :: ArrowXml a => QName -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasQName tag)
