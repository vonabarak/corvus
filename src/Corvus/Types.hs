{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Corvus.Types
  ( -- * Server State
    ServerState (..)
  , newServerState
  , runServerLogging
  , runFilteredLogging

    -- * Configuration
  , ServerConfig (..)
  , defaultServerConfig

    -- * Listen Address
  , ListenAddress (..)
  , getDefaultSocketPath
  )
where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.Logger (LogLevel (..), LoggingT, filterLogger, runStdoutLoggingT)
import Corvus.Qemu.Config (QemuConfig)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Postgresql (SqlBackend)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Shared server state
data ServerState = ServerState
  { ssStartTime :: !UTCTime
  -- ^ When the server started
  , ssConnectionCount :: TVar Int
  -- ^ Current connection count
  , ssShutdownFlag :: TVar Bool
  -- ^ Signal to shutdown
  , ssDbPool :: Pool SqlBackend
  -- ^ Database connection pool
  , ssQemuConfig :: !QemuConfig
  -- ^ QEMU configuration
  , ssLogLevel :: !LogLevel
  -- ^ Minimum log level for handler logging
  }

-- | Create a new server state
newServerState :: Pool SqlBackend -> QemuConfig -> IO ServerState
newServerState pool qemuConfig = do
  startTime <- getCurrentTime
  connCount <- newTVarIO 0
  shutdownFlag <- newTVarIO False
  pure
    ServerState
      { ssStartTime = startTime
      , ssConnectionCount = connCount
      , ssShutdownFlag = shutdownFlag
      , ssDbPool = pool
      , ssQemuConfig = qemuConfig
      , ssLogLevel = LevelInfo
      }

-- | Run a LoggingT action filtered to the server's minimum log level
runServerLogging :: ServerState -> LoggingT IO a -> IO a
runServerLogging state = runFilteredLogging (ssLogLevel state)

-- | Run a LoggingT action filtered to a minimum log level
runFilteredLogging :: LogLevel -> LoggingT IO a -> IO a
runFilteredLogging minLevel =
  runStdoutLoggingT . filterLogger (\_ level -> level >= minLevel)

-- | Server configuration
data ServerConfig = ServerConfig
  { scHost :: !Text
  -- ^ Host to bind to
  , scPort :: !Int
  -- ^ Port to listen on
  , scUnixSocket :: Maybe FilePath
  -- ^ Optional Unix socket path
  , scDbUri :: !Text
  -- ^ PostgreSQL connection URI
  }
  deriving (Eq, Show)

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { scHost = "127.0.0.1"
    , scPort = 9876
    , scUnixSocket = Nothing
    , scDbUri = "postgresql://localhost/corvus"
    }

-- | Address to listen on or connect to
data ListenAddress
  = -- | TCP host and port
    TcpAddress !String !Int
  | -- | Unix socket path
    UnixAddress !FilePath
  deriving (Eq, Show)

-- | Get the default socket path ($XDG_RUNTIME_DIR/corvus/corvus.sock)
-- Falls back to /tmp/corvus/corvus.sock if XDG_RUNTIME_DIR is not set
getDefaultSocketPath :: IO FilePath
getDefaultSocketPath = do
  mRuntimeDir <- lookupEnv "XDG_RUNTIME_DIR"
  let baseDir = fromMaybe "/tmp" mRuntimeDir
  pure $ baseDir </> "corvus" </> "corvus.sock"
