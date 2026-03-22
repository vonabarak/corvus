{-# LANGUAGE OverloadedStrings #-}

-- | Client configuration.
module Corvus.Client.Config
  ( ClientConfig (..)
  , defaultClientConfig
  )
where

-- | Client configuration
newtype ClientConfig = ClientConfig
  { ccRemoteViewer :: FilePath
  -- ^ Path to remote-viewer executable
  }
  deriving (Eq, Show)

-- | Default client configuration
defaultClientConfig :: ClientConfig
defaultClientConfig =
  ClientConfig
    { ccRemoteViewer = "remote-viewer"
    }
