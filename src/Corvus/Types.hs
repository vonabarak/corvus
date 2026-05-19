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

import qualified Capnp as C
import qualified Capnp.Gen.Streams as CGS
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM (TMVar, TVar, newTVarIO)
import Control.Monad.Logger (LogLevel (..), LoggingT, filterLogger, runStdoutLoggingT)
import Corvus.NetAgentClient (NetAgentClient)
import Corvus.NodeAgentClient (NodeAgentClient)
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
  , ssNetAgent :: TVar (Maybe NetAgentClient)
  -- ^ Link to the privileged @corvus-netd@ agent. Set by the
  -- connect-and-hold async in 'app/daemon/Main.hs' once the
  -- agent is reachable; cleared if the connection drops.
  -- Network / NetIf handlers consult this TVar and hard-error
  -- with @netd unavailable@ when it's 'Nothing'.
  , ssNodeAgent :: TVar (Maybe NodeAgentClient)
  -- ^ Link to the per-host @corvus-nodeagent@. Same pattern as
  -- 'ssNetAgent'. Phase 1 ships the connection only; later
  -- phases route disk / VM / console operations through this
  -- handle.
  , ssGuestAgentSubs :: TVar (Map.Map Int64 [C.Client CGS.GuestAgentStatusSink])
  -- ^ Per-VM 'vm.subscribeGuestAgent' subscriber lists. The
  -- guest-agent poller pushes a 'GuestAgentStatus' to each sink
  -- after every poll cycle; dead sinks are pruned on the next
  -- push attempt.
  , ssTaskProgressSubs :: TVar (Map.Map Int64 [C.Client CGS.TaskProgressSink])
  -- ^ Per-task-id 'taskManager.subscribe' subscriber lists.
  -- The Action runtime pushes a 'TaskProgressEvent' to each
  -- sink at task transitions (started / finished); dead sinks
  -- are pruned on the next push attempt.
  , ssVsockCidLock :: !(MVar ())
  -- ^ Serialises VSOCK CID allocation across concurrent VM
  -- create/start handlers. The allocator reads the DB, picks a
  -- candidate, probes the host kernel, then the caller persists
  -- the result — all of that has to be atomic within the daemon
  -- or two parallel handlers race onto the same CID and one
  -- QEMU process fails with EADDRINUSE. See
  -- 'Corvus.Node.VsockCid.withAllocatedVsockCid'.
  , ssSpicePortLock :: !(MVar ())
  -- ^ Serialises SPICE TCP-port allocation. Same shape as
  -- 'ssVsockCidLock': allocator + caller persist need to be
  -- atomic so two parallel VM starts don't pick the same port.
  -- See 'Corvus.Node.SpicePort.withAllocatedSpicePort'.
  }

-- | Create a new server state
newServerState :: Pool SqlBackend -> QemuConfig -> IO ServerState
newServerState pool qemuConfig = do
  startTime <- getCurrentTime
  connCount <- newTVarIO 0
  shutdownFlag <- newTVarIO False
  netAgent <- newTVarIO Nothing
  nodeAgent <- newTVarIO Nothing
  gaSubs <- newTVarIO Map.empty
  taskSubs <- newTVarIO Map.empty
  vsockLock <- newMVar ()
  spiceLock <- newMVar ()
  pure
    ServerState
      { ssStartTime = startTime
      , ssConnectionCount = connCount
      , ssShutdownFlag = shutdownFlag
      , ssDbPool = pool
      , ssQemuConfig = qemuConfig
      , ssLogLevel = LevelInfo
      , ssNetAgent = netAgent
      , ssNodeAgent = nodeAgent
      , ssGuestAgentSubs = gaSubs
      , ssTaskProgressSubs = taskSubs
      , ssVsockCidLock = vsockLock
      , ssSpicePortLock = spiceLock
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
