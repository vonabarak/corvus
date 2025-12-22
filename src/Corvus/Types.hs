{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Corvus.Types
  ( -- * Server State
    ServerState (..),
    newServerState,

    -- * Configuration
    ServerConfig (..),
    defaultServerConfig,
  )
where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Postgresql (SqlBackend)

-- | Shared server state
data ServerState = ServerState
  { -- | When the server started
    ssStartTime :: !UTCTime,
    -- | Current connection count
    ssConnectionCount :: TVar Int,
    -- | Signal to shutdown
    ssShutdownFlag :: TVar Bool,
    -- | Database connection pool
    ssDbPool :: Pool SqlBackend
  }

-- | Create a new server state
newServerState :: Pool SqlBackend -> IO ServerState
newServerState pool = do
  startTime <- getCurrentTime
  connCount <- newTVarIO 0
  shutdownFlag <- newTVarIO False
  pure
    ServerState
      { ssStartTime = startTime,
        ssConnectionCount = connCount,
        ssShutdownFlag = shutdownFlag,
        ssDbPool = pool
      }

-- | Server configuration
data ServerConfig = ServerConfig
  { -- | Host to bind to
    scHost :: !Text,
    -- | Port to listen on
    scPort :: !Int,
    -- | Optional Unix socket path
    scUnixSocket :: Maybe FilePath,
    -- | PostgreSQL connection URI
    scDbUri :: !Text
  }
  deriving (Eq, Show)

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { scHost = "127.0.0.1",
      scPort = 9876,
      scUnixSocket = Nothing,
      scDbUri = "postgresql://localhost/corvus"
    }
