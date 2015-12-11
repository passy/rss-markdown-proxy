{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( server
    , selectDescriptions
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

server' :: IO ()
server' =
  S.scotty 3000 $
    S.get "/feed.rss" $ do
      res <- liftIO $ fetchFeed feedUrl
      let doc = readString [withWarnings yes] $ T.unpack res
      descs <- liftIO $ runX . xshow $ doc >>> processChildren (selectDescriptions >>> changeText (const "wat"))
      S.text $ T.pack $ show descs

infixr 5 />/
(/>/) :: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlTree -> a XmlTree XmlTree
pred />/ action = processChildren action `when` pred

server :: IO ()
server = do
  rss <- readFile "test/fixtures/sounds.rss"
  let doc = readString [withWarnings yes] rss
  _ <- runX . xshow $
    doc
    >>> propagateNamespaces
    >>> processTopDown processFeed
    >>> indentDoc
    >>> writeDocument [] ""
  return ()

processFeed :: ArrowXml a => a XmlTree XmlTree
processFeed = (isElem >>> hasName "item") />/ selectDescriptions />/ changeText (const "***WOOT***")

fetchFeed :: String -> IO T.Text
fetchFeed url = do
  r <- W.get url
  return $ TE.decodeUtf8 $ r ^. W.responseBody

selectDescriptions :: ArrowXml a => a XmlTree XmlTree
selectDescriptions =
  let summaryQName = mkNsName "summary" itunesNs
  in isElem >>> (hasQName summaryQName <+> hasName "description")
