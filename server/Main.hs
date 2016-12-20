{-# LANGUAGE RankNTypes #-}

module Main where

import Lib.Server (server)
import Lib.Types (Port(..), ServerOptions(..))

import Data.Default (def)
import Data.Version (Version(), showVersion)
import Options.Applicative
import Paths_rss_markdown_proxy (version)

readPort :: forall a. ReadM (Port a)
readPort = eitherReader $ \arg -> case reads arg of
  [(r, "")] -> return $ Port r
  _         -> Left $ "cannot parse port value `" <> arg <> "'"

serverParser :: Version -> ParserInfo ServerOptions
serverParser ver =
  info ( helper <*> appOpts <**> versionInfo )
    ( fullDesc
   <> progDesc "Reverse proxy for rendering Markdown in RSS feeds"
   <> header "rss-markdown-proxy-server" )
  where
    appOpts = ServerOptions
      <$> argument str (metavar "URL")
      <*> option readPort
          ( long "port"
         <> short 'p'
         <> value def
         <> showDefault
         <> help "HTTP proxy server port" )
      <*> option readPort
          ( long "metrics-port"
         <> short 'm'
         <> value def
         <> showDefault
         <> help "HTTP metrics server port" )

    versionInfo = infoOption ( "rss-markdown-proxy-server " ++ showVersion ver )
      ( short 'V'
     <> long "version"
     <> hidden
     <> help "Show version information" )

main :: IO ()
main = execParser (serverParser version) >>= server
