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
  , newAutostartFlags
  , claimAutostartSlot
  , clearNodeConn
  , clearNetConn
  , removeNodeConns

    -- * Scheduler reservations
  , reserveRam
  , reservedRamFor
  , clearReservation

    -- * VM stats cache
  , vmStatsRingCapacity

    -- * Per-node serialisation locks
  , vsockCidLockFor

    -- * Task cancellation registry
  , newTaskCancelToken
  , registerTaskThread
  , unregisterTask
  , lookupTaskCancelToken
  , lookupTaskThread

    -- * Socket Buffer Types
  , SocketBuffer (..)
  , SocketBufferHandle (..)

    -- * Listen Address
  , ListenAddress (..)
  , getDefaultSocketPath
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as CGS
import qualified Capnp.Gen.Vm as CGVm
import Control.Concurrent (ThreadId)
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
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (fromSqlKey)
import Network.Socket (Socket)
import System.Environment (lookupEnv)

import System.FilePath ((</>))

-- | Bound on the per-VM resource-stats ring buffer in
-- 'ssVmStatsRing'. 60 samples at the agent's 10-second poll
-- cadence is exactly 10 minutes — what the WebUI needs to seed
-- its sparkline panel on page load. Memory cost is small:
-- 60 × ~100 bytes × N VMs (≈600 KiB at 100 VMs); buffers are
-- cleared when a VM transitions out of running.
vmStatsRingCapacity :: Int
vmStatsRingCapacity = 60

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
  , ssVmStatsRing :: TVar (Map.Map Int64 (Seq.Seq (C.Parsed CGVm.VmStats)))
  -- ^ Per-VM resource-stats ring buffer (most recent 60 samples
  -- == 10 minutes at the 10s poll cadence). The daemon-side
  -- 'DaemonVmStatusSink' appends to the right on every agent
  -- push; bounded by a trim to 'vmStatsRingCapacity'.
  -- 'vm.show' reads the rightmost entry; 'vm.getStatsHistory'
  -- returns the whole sequence (oldest first).
  , ssVmStatsSubs :: TVar (Map.Map Int64 [C.Client CGVm.VmStatsSink])
  -- ^ Per-VM 'vm.subscribeStats' subscriber lists. Each agent
  -- snapshot fans out the matching entry's 'VmStats' to every
  -- subscriber; dead sinks are pruned on the next push attempt.
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
  , ssTaskCancels :: TVar (Map.Map Int64 (TVar Bool))
  -- ^ Cooperative cancellation tokens keyed by top-level task id.
  -- 'TaskManager.cancel' flips the flag 'True'; the action runtime
  -- threads the same 'TVar' through the task's whole subtask tree
  -- (subtasks inherit the parent's token), and orchestrator loops
  -- poll it at step boundaries via 'throwIfCancelled'. Entry is
  -- created when a top-level task starts and removed when it
  -- finalises. See "Corvus.Action".
  , ssTaskThreads :: TVar (Map.Map Int64 ThreadId)
  -- ^ Worker 'ThreadId' for each top-level task, keyed by task id.
  -- The hard-cancel fallback: 'TaskManager.cancel' @throwTo@s a
  -- 'TaskCancelledException' here to interrupt an action blocked in
  -- an in-flight RPC that no cooperative checkpoint can reach.
  -- Same lifecycle as 'ssTaskCancels'.
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
  , ncVmAutostartFired :: !(TVar Bool)
  -- ^ \"Has the VM-autostart loop already fired for this node\
  -- since this supervisor started?\" Flipped True on the first
  -- successful nodeagent connect; reset to False only when the
  -- supervisor is torn down + respawned (e.g. node delete or
  -- a re-registration). Prevents an unrelated nodeagent flap
  -- from re-issuing 'vm start' on autostart VMs that the
  -- operator may have stopped post-startup.
  , ncNetAutostartFired :: !(TVar Bool)
  -- ^ Same gate as 'ncVmAutostartFired' but for the netd /
  -- network-autostart side.
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
  vmStatsRing <- newTVarIO Map.empty
  vmStatsSubs <- newTVarIO Map.empty
  vsockLocks <- newTVarIO Map.empty
  spiceLock <- newMVar ()
  reservedRam <- newTVarIO Map.empty
  taskCancels <- newTVarIO Map.empty
  taskThreads <- newTVarIO Map.empty
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
      , ssVmStatsRing = vmStatsRing
      , ssVmStatsSubs = vmStatsSubs
      , ssVsockCidLocks = vsockLocks
      , ssSpicePortLock = spiceLock
      , ssTaskCancels = taskCancels
      , ssTaskThreads = taskThreads
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

-- | Allocate the two autostart-fired TVars a fresh 'NodeConns'
-- needs. Separated from the record literal so the supervisor's
-- call site stays terse and the default ('False') is in one
-- place. Both flags start unfired; the supervisor's onConnect
-- callbacks flip them to True via 'claimAutostartSlot' on the
-- first successful agent dial.
newAutostartFlags :: IO (TVar Bool, TVar Bool)
newAutostartFlags = do
  vm <- newTVarIO False
  net <- newTVarIO False
  pure (vm, net)

-- | Race-free \"fire this autostart kind once per supervisor
-- lifetime\". Returns 'True' on the first call for the given node
-- + flag accessor; 'False' on every subsequent call. The atomic
-- read-modify-write happens inside a single 'atomically' so two
-- onConnect callbacks racing for the same flag (impossible in
-- practice — each agent has its own loop — but cheap to make
-- safe) can't both observe 'True'.
--
-- If the node has no 'NodeConns' entry yet (node was deleted
-- between the supervisor's connect and this call), returns
-- 'False' so the caller skips the autostart work harmlessly.
claimAutostartSlot
  :: ServerState
  -> M.NodeId
  -> (NodeConns -> TVar Bool)
  -> IO Bool
claimAutostartSlot state nid flagOf = do
  m <- readTVarIO (ssAgents state)
  case Map.lookup nid m of
    Nothing -> pure False
    Just nc ->
      atomically $ do
        fired <- readTVar (flagOf nc)
        if fired
          then pure False
          else do
            writeTVar (flagOf nc) True
            pure True

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

-- | Create and register a fresh (unset) cancellation token for a
-- top-level task. Call once when the task starts; the action
-- runtime threads the returned 'TVar' through the task's subtask
-- tree. Idempotent-ish: a second call replaces the entry.
newTaskCancelToken :: ServerState -> Int64 -> IO (TVar Bool)
newTaskCancelToken state tid = do
  tok <- newTVarIO False
  atomically $ modifyTVar' (ssTaskCancels state) (Map.insert tid tok)
  pure tok

-- | Record the worker thread for a top-level task (for hard cancel).
registerTaskThread :: ServerState -> Int64 -> ThreadId -> IO ()
registerTaskThread state tid th =
  atomically $ modifyTVar' (ssTaskThreads state) (Map.insert tid th)

-- | Drop a task's cancellation token + thread entry. Called when the
-- task finalises. A no-op for ids that were never registered (e.g.
-- subtasks, which share their parent's token).
unregisterTask :: ServerState -> Int64 -> IO ()
unregisterTask state tid = atomically $ do
  modifyTVar' (ssTaskCancels state) (Map.delete tid)
  modifyTVar' (ssTaskThreads state) (Map.delete tid)

-- | Look up a top-level task's cancellation token, if it is running.
lookupTaskCancelToken :: ServerState -> Int64 -> IO (Maybe (TVar Bool))
lookupTaskCancelToken state tid =
  Map.lookup tid <$> readTVarIO (ssTaskCancels state)

-- | Look up a top-level task's worker thread, if it is running.
lookupTaskThread :: ServerState -> Int64 -> IO (Maybe ThreadId)
lookupTaskThread state tid =
  Map.lookup tid <$> readTVarIO (ssTaskThreads state)

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
