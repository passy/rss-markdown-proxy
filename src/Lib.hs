{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( selectDescriptions
    , fetchFeed
    , transformRSS
    ) where

import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Encoding       as TE
import qualified Network.Wreq                  as W

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Markdown                 (def, markdown)

import           Control.Lens                  hiding (deep)
import           Text.XML.HXT.Core

itunesNs :: String
itunesNs = "http://www.itunes.com/dtds/podcast-1.0.dtd"

infixr 5 />/
(/>/) :: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlTree -> a XmlTree XmlTree
predicate />/ action = processChildren action `when` predicate

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
