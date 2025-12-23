{-# LANGUAGE OverloadedStrings #-}

-- | Client configuration.
module Corvus.Client.Config
  ( ClientConfig (..),
    defaultClientConfig,
  )
where

-- | Client configuration
newtype ClientConfig = ClientConfig
  { -- | Path to remote-viewer executable
    ccRemoteViewer :: FilePath
  }
  deriving (Eq, Show)

-- | Default client configuration
defaultClientConfig :: ClientConfig
defaultClientConfig =
  ClientConfig
    { ccRemoteViewer = "remote-viewer"
    }
