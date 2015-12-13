{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where

import qualified Data.Text.Lazy     as T
import           Lib                (fetchFeed, transformRSS)

import           Data.Version             (Version (), showVersion)
import           Options.Applicative
import           Paths_rss_markdown_proxy (version)

data CliOptions = CliOptions
  { path :: Maybe FilePath
  , url  :: Maybe String
  }

cliParser :: Version -> ParserInfo CliOptions
cliParser ver =
  info ( helper <*> appOpts <**> versionInfo )
    ( fullDesc
   <> progDesc "Transform an RSS feed to render Markdown to HTML in description fields"
   <> header "rss-markdown-proxy" )
  where
    appOpts = CliOptions
      <$> optional ( argument str (metavar "FILE") )
      <*> optional ( strOption
          ( long "url"
         <> short 'u'
         <> help "Remote URL to fetch" ) )

    versionInfo = infoOption ( "rss-markdown-proxy " ++ showVersion ver )
      ( short 'V'
     <> long "version"
     <> hidden
     <> help "Show version information" )

main :: IO ()
main =
  execParser (cliParser version) >>= run
  where
    run :: CliOptions -> IO ()
    run opts =
      case url opts of
        Nothing -> transformFile opts
        Just u -> T.unpack <$> fetchFeed u >>= transformRSS >>= putStrLn

    transformFile :: CliOptions -> IO ()
    transformFile opts =
      putStrLn =<< case path opts of
        Nothing -> transformRSS =<< getContents
        Just path' -> transformRSS =<< readFile path'
