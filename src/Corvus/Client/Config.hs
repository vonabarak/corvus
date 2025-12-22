{-# LANGUAGE OverloadedStrings #-}

-- | Client configuration.
module Corvus.Client.Config
  ( ClientConfig (..),
    defaultClientConfig,
  )
where

-- | Client configuration
data ClientConfig = ClientConfig
  { -- | Daemon host
    ccHost :: String,
    -- | Daemon port
    ccPort :: Int,
    -- | Path to remote-viewer executable
    ccRemoteViewer :: FilePath
  }
  deriving (Eq, Show)

-- | Default client configuration
defaultClientConfig :: ClientConfig
defaultClientConfig =
  ClientConfig
    { ccHost = "127.0.0.1",
      ccPort = 9876,
      ccRemoteViewer = "remote-viewer"
    }
