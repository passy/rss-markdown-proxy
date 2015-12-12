{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( server
    , selectDescriptions
    ) where

import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Encoding       as TE
import qualified Network.Wreq                  as W
import qualified Web.Scotty                    as S

import           Control.Monad.IO.Class        (liftIO)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Markdown                 (def, markdown)

import           Control.Lens                  hiding (deep)
import           Text.XML.HXT.Core

feedUrl :: String
feedUrl = "http://feeds.soundcloud.com/users/soundcloud:users:189413584/sounds.rss"

itunesNs :: String
itunesNs = "http://www.itunes.com/dtds/podcast-1.0.dtd"

infixr 5 />/
(/>/) :: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlTree -> a XmlTree XmlTree
pred />/ action = processChildren action `when` pred

server :: IO ()
server = do
  rss <- readFile "test/fixtures/sounds.rss"
  transformRSS rss >>= putStrLn

server' :: IO ()
server' =
  S.scotty 3000 $
    S.get "/feed.rss" $ do
      res <- liftIO $ transformRSS =<< T.unpack <$> fetchFeed feedUrl
      S.text $ T.pack res

transformRSS :: String -> IO String
transformRSS input = do
  let doc = readString [withWarnings yes] input
  [res] <- runX $
    doc
    >>> propagateNamespaces
    >>> processTopDown processFeed
    >>> indentDoc
    >>> writeDocumentToString []
  return res

processFeed :: ArrowXml a => a XmlTree XmlTree
processFeed = (isElem >>> hasName "item")
  />/ selectDescriptions
  />/ (getText >>> arr renderMarkdownToHtml >>> mkCdata)

renderMarkdownToHtml :: String -> String
renderMarkdownToHtml = T.pack >>> markdown def >>> renderHtml >>> T.unpack

fetchFeed :: String -> IO T.Text
fetchFeed url = do
  r <- W.get url
  return $ TE.decodeUtf8 $ r ^. W.responseBody

selectDescriptions :: ArrowXml a => a XmlTree XmlTree
selectDescriptions =
  let summaryQName = mkNsName "summary" itunesNs
  in isElem >>> (hasQName summaryQName <+> hasName "description")
