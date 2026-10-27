{-# LANGUAGE OverloadedStrings #-}

-- | Client configuration.
module Corvus.Client.Config
  ( ClientConfig (..)
  , defaultClientConfig
  )
where

-- | Client configuration
data ClientConfig = ClientConfig
  { ccRemmina :: FilePath
  -- ^ Path to Remmina executable
  , ccRemoteViewer :: FilePath
  -- ^ Path to remote-viewer executable
  }
  deriving stock (Eq, Show)

-- | Default client configuration
defaultClientConfig :: ClientConfig
defaultClientConfig =
  ClientConfig
    { ccRemmina = "remmina"
    , ccRemoteViewer = "remote-viewer"
    }
