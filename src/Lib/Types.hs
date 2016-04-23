{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.Types
  ( Port(Port)
  , ServerOptions(ServerOptions)
  , unPort
  , Metrics
  , Server
  , metricsPort
  , port
  , url
  ) where

import           Control.Lens
import           Data.Default             (Default (), def)

newtype Port a = Port Int
  deriving (Read, Show)

unPort :: Port a -> Int
unPort (Port i) = i

data Server
data Metrics

-- | Command line options provided to start up the server.
data ServerOptions = ServerOptions
  { _url         :: String
  , _port        :: Port Server
  , _metricsPort :: Port Metrics
  }

makeLenses ''ServerOptions

instance Default (Port Server) where
  def = Port 3000

instance Default (Port Metrics) where
  def = Port 3001
