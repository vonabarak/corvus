{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Corvus.Types
  ( -- * Server State
    ServerState (..)
  , newServerState
  , runServerLogging
  , runFilteredLogging

    -- * Socket Buffer Types
  , SocketBuffer (..)
  , SocketBufferHandle (..)

    -- * Configuration
  , ServerConfig (..)
  , defaultServerConfig

    -- * Listen Address
  , ListenAddress (..)
  , getDefaultSocketPath
  )
where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TMVar, TVar, newTVarIO)
import Control.Monad.Logger (LogLevel (..), LoggingT, filterLogger, runStdoutLoggingT)
import Corvus.Qemu.Config (QemuConfig)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Postgresql (SqlBackend)
import Network.Socket (Socket)
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
  , ssNamespacePid :: TVar (Maybe Int)
  -- ^ PID of the global network namespace manager
  , ssPastaPid :: TVar (Maybe Int)
  -- ^ PID of the pasta process (for NAT)
  , ssSerialBuffers :: TVar (Map.Map Int64 SocketBufferHandle)
  -- ^ Per-VM serial console ring buffers (headless VMs only)
  , ssMonitorBuffers :: TVar (Map.Map Int64 SocketBufferHandle)
  -- ^ Per-VM HMP monitor ring buffers (all running VMs)
  , ssGuestAgentConns :: TVar (Map.Map Int64 (MVar (Maybe Socket)))
  -- ^ Per-VM persistent guest agent connections.
  -- QEMU's chardev only supports one connection at a time (listen backlog=1).
  -- The MVar serializes access and holds the socket between operations.
  -- Nothing = not connected (will connect on next use).
  -- Just sock = persistent connection ready for commands.
  }

-- | Create a new server state
newServerState :: Pool SqlBackend -> QemuConfig -> IO ServerState
newServerState pool qemuConfig = do
  startTime <- getCurrentTime
  connCount <- newTVarIO 0
  shutdownFlag <- newTVarIO False
  namespacePid <- newTVarIO Nothing
  pastaPid <- newTVarIO Nothing
  serialBuffers <- newTVarIO Map.empty
  monitorBuffers <- newTVarIO Map.empty
  gaLocks <- newTVarIO Map.empty
  pure
    ServerState
      { ssStartTime = startTime
      , ssConnectionCount = connCount
      , ssShutdownFlag = shutdownFlag
      , ssDbPool = pool
      , ssQemuConfig = qemuConfig
      , ssLogLevel = LevelInfo
      , ssNamespacePid = namespacePid
      , ssPastaPid = pastaPid
      , ssSerialBuffers = serialBuffers
      , ssMonitorBuffers = monitorBuffers
      , ssGuestAgentConns = gaLocks
      }

--------------------------------------------------------------------------------
-- Socket Buffer Types
--------------------------------------------------------------------------------

-- | Ring buffer for output from a QEMU chardev Unix socket.
-- Used for both headless serial consoles and the HMP monitor.
data SocketBuffer = SocketBuffer
  { sbData :: !(TVar BS.ByteString)
  -- ^ Current buffer contents (truncated to capacity)
  , sbTotalWritten :: !(TVar Int64)
  -- ^ Monotonically increasing total bytes written
  , sbNotify :: !(TMVar ())
  -- ^ Signaled when new data is written
  , sbCapacity :: !Int
  -- ^ Maximum buffer size in bytes
  }

-- | Handle stored in ServerState for each live QEMU chardev buffer.
data SocketBufferHandle = SocketBufferHandle
  { sbhBuffer :: !SocketBuffer
  -- ^ The ring buffer
  , sbhQemuSock :: !(TVar (Maybe Socket))
  -- ^ QEMU chardev socket (for writing client input); Nothing if disconnected
  , sbhShutdown :: !(TVar Bool)
  -- ^ Set when QEMU disconnects
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
