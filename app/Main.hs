module Main where

import Lib.Server (server)

import Options.Applicative
import           Paths_rss_markdown_proxy (version)
import Data.Version (Version(), showVersion)
import Data.Monoid ((<>))
import Data.Default (Default(), def)

newtype Port = Port Int
  deriving (Read, Show)

-- | Command line options provided to start up the server.
data ServerOptions = ServerOptions
  { url :: String
  , port :: Port
  }

instance Default Port where
  def = Port 3000

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
main =
  execParser (serverParser version) >>= run
  where
    run :: ServerOptions -> IO ()
    run _ = server
