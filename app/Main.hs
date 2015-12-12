module Main where

import Lib.Server (server, ServerOptions(..), Port(..))

import Options.Applicative
import           Paths_rss_markdown_proxy (version)
import Data.Version (Version(), showVersion)
import Data.Monoid ((<>))
import Data.Default (Default(), def)

readPort :: ReadM Port
readPort = eitherReader $ \arg -> case reads arg of
  [(r, "")] -> return $ Port r
  _         -> Left $ "cannot parse port value `" <> arg <> "'"

serverParser :: Version -> ParserInfo ServerOptions
serverParser ver =
  info ( helper <*> appOpts <**> versionInfo )
    ( fullDesc
   <> progDesc "Reverse proxy for rendering Markdown in RSS feeds"
   <> header "rss-markdown-proxy" )
  where
    appOpts = ServerOptions
      <$> argument str (metavar "URL")
      <*> option readPort
          ( long "port"
         <> short 'p'
         <> value def
         <> showDefault
         <> help "Port" )

    versionInfo = infoOption ( "rss-markdown-proxy " ++ showVersion ver )
      ( short 'V'
     <> long "version"
     <> hidden
     <> help "Show version information" )

main :: IO ()
main = execParser (serverParser version) >>= server
