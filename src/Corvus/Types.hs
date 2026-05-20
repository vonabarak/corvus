{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Corvus.Types
  ( -- * Server State
    ServerState (..)
  , NodeConns (..)
  , newServerState
  , runServerLogging
  , runFilteredLogging

    -- * Per-node agent routing
  , withNodeAgent
  , withNetAgent
  , lookupNodeAgent
  , lookupNetAgentMaybe
  , registerNodeConns
  , clearNodeConn
  , clearNetConn
  , removeNodeConns

    -- * Scheduler reservations
  , reserveRam
  , reservedRamFor
  , clearReservation

    -- * Per-node serialisation locks
  , vsockCidLockFor

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
import Control.Concurrent.Async (Async)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM (TMVar, TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad.Logger (LogLevel (..), LoggingT, filterLogger, runStdoutLoggingT)
import qualified Corvus.Model as M
import Corvus.NetAgentClient (NetAgentClient)
import Corvus.NodeAgentClient (NodeAgentClient)
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Tls (TlsConfig)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (fromSqlKey)
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
  , ssAgents :: TVar (Map.Map M.NodeId NodeConns)
  -- ^ Per-node registry of live agent connections. The daemon
  -- forks one reconnect-loop per row in the 'Node' table at
  -- startup; each loop dials the node's @corvus-nodeagent@
  -- (@nodeAgentPort@) and @corvus-netd@ (@netAgentPort@) and
  -- updates the matching 'NodeConns' entry. Handlers route
  -- their agent calls through 'withNodeAgent' / 'withNetAgent'
  -- which hard-error when the entry is missing or its agent
  -- field is 'Nothing'.
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
  , ssVsockCidLocks :: TVar (Map.Map M.NodeId (MVar ()))
  -- ^ Per-node VSOCK CID allocation serialisation. The
  -- allocator picks a candidate, asks the matching node's
  -- agent to probe its kernel, then the caller persists — all
  -- atomic within the daemon for that node. Two parallel
  -- handlers targeting *different* nodes don't contend. See
  -- 'Corvus.Node.VsockCid.withAllocatedVsockCid'.
  , ssSpicePortLock :: !(MVar ())
  -- ^ Serialises SPICE TCP-port allocation. Same shape as
  -- 'ssVsockCidLock': allocator + caller persist need to be
  -- atomic so two parallel VM starts don't pick the same port.
  -- See 'Corvus.Node.SpicePort.withAllocatedSpicePort'.
  , ssReservedRam :: TVar (Map.Map M.NodeId Int)
  -- ^ Scheduler reservation bookkeeping: RAM (MiB) the
  -- scheduler has handed out for VMs created via 'pickNodeForVm'
  -- since the last agent push reset the relevant entry. The
  -- scheduler subtracts the per-node reservation from each
  -- node's reported @ramMbFree@ when scoring candidates, so
  -- back-to-back creates within the same daemon don't all
  -- pile onto the same (now-no-longer-empty) node before the
  -- agent's next push reflects reality. The Phase 5 agent
  -- 'NodeStats' push hook clears the entry; until then
  -- entries persist for the daemon's lifetime, which is fine
  -- in practice because the typical operator session creates
  -- a small number of VMs and a real RAM cap is enforced by
  -- the agent itself on @vmStart@.
  , ssTlsConfig :: !(Maybe TlsConfig)
  -- ^ Loaded mutual-TLS material for the daemon. Carries the
  -- daemon's own cert / key / CA store and the CN-prefix
  -- expectation for inbound CLI connections (peer must be
  -- @corvus-client:*@). 'Nothing' when @--no-tls@ was passed;
  -- in that case every TCP listener and outbound dial falls
  -- back to plain sockets. For outbound dials to agents, the
  -- supervisor uses 'Corvus.Tls.withPeerExpectation' to swap
  -- the peer expectation to @corvus-node:<name>@ /
  -- @corvus-netd:<name>@.
  }

-- | Per-node bundle of live agent connections plus the
-- supervisor 'Async' that ran the reconnect loop. Both agent
-- fields start as 'Nothing'; the supervisor populates them once
-- the respective TCP dial succeeds and clears them when the
-- connection drops.
data NodeConns = NodeConns
  { ncNodeAgent :: !(Maybe NodeAgentClient)
  -- ^ Live nodeagent cap for this node, or 'Nothing' while
  -- disconnected / unreachable.
  , ncNetAgent :: !(Maybe NetAgentClient)
  -- ^ Live netd cap for this node.
  , ncSupervisor :: !(Async ())
  -- ^ The async that holds both connections open and reconnects
  -- on drop. Cancelled when the node is deleted via @crv node
  -- delete@ or when the daemon shuts down.
  }

-- | Create a new server state
newServerState :: Pool SqlBackend -> QemuConfig -> IO ServerState
newServerState pool qemuConfig = do
  startTime <- getCurrentTime
  connCount <- newTVarIO 0
  shutdownFlag <- newTVarIO False
  agents <- newTVarIO Map.empty
  gaSubs <- newTVarIO Map.empty
  taskSubs <- newTVarIO Map.empty
  vsockLocks <- newTVarIO Map.empty
  spiceLock <- newMVar ()
  reservedRam <- newTVarIO Map.empty
  pure
    ServerState
      { ssStartTime = startTime
      , ssConnectionCount = connCount
      , ssShutdownFlag = shutdownFlag
      , ssDbPool = pool
      , ssQemuConfig = qemuConfig
      , ssLogLevel = LevelInfo
      , ssAgents = agents
      , ssGuestAgentSubs = gaSubs
      , ssTaskProgressSubs = taskSubs
      , ssVsockCidLocks = vsockLocks
      , ssSpicePortLock = spiceLock
      , ssReservedRam = reservedRam
      , ssTlsConfig = Nothing
      }

-- | Look up the nodeagent cap for a node. Returns 'Left' with a
-- diagnostic message when the node isn't registered with the
-- daemon (i.e. no supervisor was spawned for it) or its
-- nodeagent connection is currently down.
lookupNodeAgent :: ServerState -> M.NodeId -> IO (Either Text NodeAgentClient)
lookupNodeAgent state nid = do
  m <- readTVarIO (ssAgents state)
  pure $ case Map.lookup nid m of
    Nothing ->
      Left $
        "node " <> T.pack (show (fromSqlKey nid)) <> " not registered with daemon"
    Just nc -> case ncNodeAgent nc of
      Nothing ->
        Left $
          "nodeagent for node "
            <> T.pack (show (fromSqlKey nid))
            <> " unavailable"
      Just nac -> Right nac

-- | Look up the nodeagent cap and run an action against it.
-- Convenience wrapper around 'lookupNodeAgent' for the common
-- "if reachable, do X" pattern in handlers.
withNodeAgent
  :: ServerState
  -> M.NodeId
  -> (NodeAgentClient -> IO a)
  -> IO (Either Text a)
withNodeAgent state nid action = do
  r <- lookupNodeAgent state nid
  case r of
    Left e -> pure (Left e)
    Right nac -> Right <$> action nac

-- | Look up the netd cap for a node and run an action.
withNetAgent
  :: ServerState
  -> M.NodeId
  -> (NetAgentClient -> IO a)
  -> IO (Either Text a)
withNetAgent state nid action = do
  m <- readTVarIO (ssAgents state)
  case Map.lookup nid m of
    Nothing ->
      pure $
        Left $
          "node " <> T.pack (show (fromSqlKey nid)) <> " not registered with daemon"
    Just nc -> case ncNetAgent nc of
      Nothing ->
        pure $
          Left $
            "netd for node "
              <> T.pack (show (fromSqlKey nid))
              <> " unavailable"
      Just nac -> Right <$> action nac

-- | Non-blocking, non-erroring netd lookup. Returns 'Nothing' if
-- the node isn't registered or its netd cap is currently down.
-- Used by best-effort code paths (e.g. assembling 'VmSpec' when a
-- managed NIC is involved but the warning case shouldn't fail the
-- caller outright).
lookupNetAgentMaybe :: ServerState -> M.NodeId -> IO (Maybe NetAgentClient)
lookupNetAgentMaybe state nid = do
  m <- readTVarIO (ssAgents state)
  pure (ncNetAgent =<< Map.lookup nid m)

-- | Install (or replace) a 'NodeConns' entry for a node. Used
-- by the per-node reconnect spawner when a new node is added.
registerNodeConns :: ServerState -> M.NodeId -> NodeConns -> IO ()
registerNodeConns state nid nc =
  atomically $ modifyTVar' (ssAgents state) (Map.insert nid nc)

-- | Clear the nodeagent half of a node's connection bundle (the
-- supervisor sets this on disconnect; another connect attempt
-- will replace it via 'updateNodeConn').
clearNodeConn :: ServerState -> M.NodeId -> IO ()
clearNodeConn state nid =
  atomically $
    modifyTVar' (ssAgents state) $
      Map.adjust (\nc -> nc {ncNodeAgent = Nothing}) nid

-- | Clear the netd half of a node's connection bundle.
clearNetConn :: ServerState -> M.NodeId -> IO ()
clearNetConn state nid =
  atomically $
    modifyTVar' (ssAgents state) $
      Map.adjust (\nc -> nc {ncNetAgent = Nothing}) nid

-- | Remove a node's entry entirely. Used when @crv node delete@
-- cancels the supervisor.
removeNodeConns :: ServerState -> M.NodeId -> IO ()
removeNodeConns state nid = do
  atomically $ modifyTVar' (ssAgents state) (Map.delete nid)
  -- Drop any pending reservation for the gone node too — keeps
  -- the scheduler's view of "RAM I've handed out" honest.
  clearReservation state nid

-- | Bump the in-memory RAM reservation for a node. Called by
-- 'pickNodeForVm' (or any explicit-placement path) right after
-- the new 'Vm' row is inserted, so the next scheduler pass
-- doesn't double-spend the same headroom.
reserveRam :: ServerState -> M.NodeId -> Int -> IO ()
reserveRam state nid ramMb =
  atomically $
    modifyTVar' (ssReservedRam state) $
      Map.insertWith (+) nid ramMb

-- | Read the current reserved-RAM total for a node (0 if none).
-- Phase-1 helper for the scheduler.
reservedRamFor :: ServerState -> M.NodeId -> IO Int
reservedRamFor state nid = do
  m <- readTVarIO (ssReservedRam state)
  pure $ Map.findWithDefault 0 nid m

-- | Reset a node's reservation back to zero. Called when fresh
-- 'NodeStats' from the agent push arrive (Phase 5) so the
-- scheduler stops double-counting reserved RAM already
-- reflected in the agent's reported @ramMbFree@.
clearReservation :: ServerState -> M.NodeId -> IO ()
clearReservation state nid =
  atomically $
    modifyTVar' (ssReservedRam state) (Map.delete nid)

-- | Get the per-node vsock CID allocation 'MVar', lazily
-- creating it the first time a caller asks. The lock is held
-- across the candidate-pick + agent-probe + DB-persist window
-- so concurrent allocations on the *same* node never collide;
-- different nodes don't contend at all.
vsockCidLockFor :: ServerState -> M.NodeId -> IO (MVar ())
vsockCidLockFor state nid = do
  m <- readTVarIO (ssVsockCidLocks state)
  case Map.lookup nid m of
    Just lk -> pure lk
    Nothing -> do
      lk <- newMVar ()
      -- Race: a concurrent caller may have created their own
      -- lock by the time we go to install ours. Re-check inside
      -- the STM transaction and keep whichever lock wins.
      atomically $ do
        m' <- readTVar (ssVsockCidLocks state)
        case Map.lookup nid m' of
          Just existing -> pure existing
          Nothing -> do
            writeTVar (ssVsockCidLocks state) (Map.insert nid lk m')
            pure lk

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
