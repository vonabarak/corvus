{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Daemon-side 'VmStatusSink' implementation.
--
-- The agent's status poller sends one 'VmStatusSnapshot' per
-- tick (~10 s); each snapshot covers every VM in the agent's
-- ledger. 'onSnapshot' walks the snapshot and, per entry:
--
--   * stamps 'Vm.healthcheck' when the guest agent answered;
--   * updates 'NetworkInterface.guestIpAddresses' from the
--     guest-reported interfaces, matching by MAC (mirrors the
--     old in-daemon @updateGuestNetworkData@);
--   * fans the per-VM 'GuestAgentStatus' out to anyone subscribed
--     via @vm.subscribeGuestAgent@ (preserves the client-facing
--     stream — only the *source* of the data changed).
--
-- The daemon's monitor thread ('Corvus.Handlers.Vm.attachVmMonitor')
-- still drives the @VmRunning → VmStopped/VmError@ transitions on
-- QEMU exit; this module only refreshes the per-VM healthcheck
-- and IP-address metadata.
module Corvus.Handlers.VmStatusSink
  ( DaemonVmStatusSink (..)
  , newDaemonVmStatusSink
  , applyNodeStats
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Streams as CGS
import qualified Capnp.Gen.Vm as CGVm
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import qualified Control.Exception as E
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logWarnN)
import qualified Corvus.Model as M
import qualified Corvus.Node.NodeStats as NS
import Corvus.Rpc.Common (handleParsed)
import Corvus.Rpc.Streams (callSink)
import Corvus.Types
  ( ServerState (..)
  , clearReservation
  , runServerLogging
  , vmStatsRingCapacity
  )
import qualified Corvus.Types
import Data.Int (Int64)
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.Persist
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import System.Timeout (timeout)

-- | Server-side handle. Owns a reference to 'ServerState' so
-- the 'onSnapshot' handler can reach the DB pool + the
-- @vm.subscribeGuestAgent@ subscriber map, plus the 'NodeId'
-- of the node whose nodeagent this sink is bound to. The
-- daemon creates one sink per node (the per-node reconnect
-- loop in @app/daemon/Main.hs@ supplies the id), so every
-- 'onSnapshot' call knows which 'Node' row to stamp.
data DaemonVmStatusSink = DaemonVmStatusSink
  { dvssState :: ServerState
  , dvssNodeId :: !M.NodeId
  }

newDaemonVmStatusSink :: ServerState -> M.NodeId -> DaemonVmStatusSink
newDaemonVmStatusSink = DaemonVmStatusSink

instance SomeServer DaemonVmStatusSink

instance CGNA.VmStatusSink'server_ DaemonVmStatusSink where
  vmStatusSink'onSnapshot (DaemonVmStatusSink state nid) =
    handleParsed $ \CGNA.VmStatusSink'onSnapshot'params {CGNA.snapshot = snap} -> do
      let CGNA.VmStatusSnapshot
            { CGNA.entries = entries
            , CGNA.nodeStats = stats
            , CGNA.snapshotAtMillis = snapMs
            } = snap
      forM_ entries (processEntry state)
      -- Per-tick node observation: stamp into the 'Node' row,
      -- bump the healthcheck timestamp, and clear the
      -- scheduler's pending RAM reservation for this node
      -- (the agent's fresh @ramMbFree@ already reflects the
      -- newly-created VM's allocation, so re-counting would
      -- double-charge headroom).
      applyNodeStats state nid stats snapMs
      pure CGNA.VmStatusSink'onSnapshot'results

-- | Reconcile one 'VmStatusEntry' with daemon state.
processEntry :: ServerState -> C.Parsed CGNA.VmStatusEntry -> IO ()
processEntry state entry = do
  let CGNA.VmStatusEntry
        { CGNA.vmId = vmId
        , CGNA.state = agentState
        , CGNA.guestAgentOk = ok
        , CGNA.lastPingMillis = pingMillis
        , CGNA.lastExitCode = exitCode
        , CGNA.netIfs = ifs
        , CGNA.stats = stats
        } = entry
      pool = ssDbPool state
  -- Stats ring: keep the most recent 'vmStatsRingCapacity' samples
  -- for running VMs, drop the ring entirely on stopped/errored.
  -- A non-running VM still pushes a zero-filled VmStats; storing
  -- those would just waste memory and confuse the WebUI.
  case agentState of
    CGNA.VmAgentState'running -> appendVmStats state vmId stats
    _ -> dropVmStatsRing state vmId
  -- Fan out the sample to live subscribers (always; subscribers
  -- can observe the running → stopped transition via the
  -- zero-filled sample).
  pushVmStats state vmId stats
  -- Stamp healthcheck on successful ping; reflect MAC→IP map in
  -- the network_interface table.
  when ok $ do
    let pingedAt = millisToUtc pingMillis
    runSqlPool (updateHealthcheck vmId pingedAt) pool
    runSqlPool (updateGuestNetworkData vmId ifs) pool
  -- Translate agent-side state into DB transitions for the
  -- subset of states the agent owns (start completion + crash
  -- detection). 'attachVmMonitor' still drives the running →
  -- stopped/error transition for VMs the agent reports as gone
  -- — keep that in one place. Here we only promote on the
  -- start-side push (forked QGA watcher in 'doVmStart' fires
  -- 'dispatchVm' the moment the first ping lands) and surface
  -- early QEMU crashes that happened before the daemon's
  -- monitor was even attached.
  case agentState of
    CGNA.VmAgentState'running
      | ok ->
          -- QGA pinged — definitively running. Promotes any
          -- 'VmStarting' or 'VmLoading' row to 'VmRunning'.
          runSqlPool (promoteStartingToRunning vmId) pool
      | otherwise ->
          -- No QGA ping (either GA disabled, or first ping not
          -- yet). For 'VmStarting' the daemon waits for QGA before
          -- promoting; for 'VmLoading' the agent's
          -- 'runIncomingStep' dispatches this snapshot AFTER QMP
          -- 'cont' succeeded — so the row is genuinely running on
          -- the QEMU side and we promote even without QGA. The
          -- helper guards on the DB's from-status so a regular
          -- steady-state tick doesn't accidentally re-write the
          -- row.
          runSqlPool (promoteLoadingToRunning vmId) pool
    CGNA.VmAgentState'errored ->
      runSqlPool (markErroredFromAgent vmId (fromIntegral exitCode)) pool
    _ -> pure ()
  -- Fan out the per-VM GuestAgentStatus to anyone subscribed via
  -- @vm.subscribeGuestAgent@. Mirrors the old in-daemon poller's
  -- 'pushGuestAgentStatus' helper.
  pushGuestAgentStatus state vmId ok pingMillis

-- | Promote @VmStarting@ or @VmLoading@ to @VmRunning@. Called on
-- the agent's QGA-ping push for GA-enabled VMs. The push channel
-- is the only place the daemon learns that the agent finished
-- bringing the VM up: the agent's 'doVmStart' forks the
-- first-QGA-ping watcher (cold boot) or its post-incoming
-- coordinator (resume from saved) and pushes a single-entry
-- snapshot the moment QEMU is live. We guard on the from-status
-- so a regular 10 s tick for a steady-state VM doesn't accidentally
-- re-write its status.
promoteStartingToRunning :: Int64 -> SqlPersistT IO ()
promoteStartingToRunning vmId = do
  let key = M.toSqlKey vmId :: M.VmId
  mVm <- get key
  case mVm of
    Just vm
      | M.vmStatus vm `elem` [M.VmStarting, M.VmLoading] ->
          update
            key
            [ M.VmStatus =. M.VmRunning
            , M.VmErrorMessage =. Nothing
            , M.VmLastErrorAt =. Nothing
            ]
    _ -> pure ()

-- | Promote @VmLoading@ to @VmRunning@ without requiring a QGA
-- ping. The agent's 'runIncomingStep' dispatches a status push
-- after QMP 'cont' succeeds on a load-from-saved VM, signalling
-- the load has completed. For non-GA VMs that's the only
-- "ready" signal — QGA-enabled VMs additionally wait for the
-- first ping via 'runGaStep' / 'promoteStartingToRunning'.
--
-- @VmStarting@ rows are NOT promoted here: a regular 10 s tick
-- on a steady-state GA-disabled running VM has @ok=false@ too,
-- but the daemon already wrote @VmRunning@ synchronously on
-- cold-no-GA start so this code path is a no-op for them.
promoteLoadingToRunning :: Int64 -> SqlPersistT IO ()
promoteLoadingToRunning vmId = do
  let key = M.toSqlKey vmId :: M.VmId
  mVm <- get key
  case mVm of
    Just vm
      | M.vmStatus vm == M.VmLoading ->
          update
            key
            [ M.VmStatus =. M.VmRunning
            , M.VmErrorMessage =. Nothing
            , M.VmLastErrorAt =. Nothing
            ]
    _ -> pure ()

-- | Surface an agent-reported errored state into the DB. Skips:
--
--   * Terminal states ('VmStopped' / 'VmError') so we don't
--     clobber whatever an earlier failure path already wrote.
--   * 'VmSaving' / 'VmMigrating' — the save executor and
--     migration orchestrator each own their own terminal flips.
--     A QEMU exit observed mid-save is expected (QMP @quit@) and
--     mustn't be reflected here.
markErroredFromAgent :: Int64 -> Int -> SqlPersistT IO ()
markErroredFromAgent vmId exitCode = do
  let key = M.toSqlKey vmId :: M.VmId
  mVm <- get key
  case mVm of
    Just vm
      | M.vmStatus vm
          `notElem` [ M.VmStopped
                    , M.VmError
                    , M.VmSaving
                    , M.VmMigrating
                    ] -> do
          now <- liftIO getCurrentTime
          let msg = "QEMU exited with code " <> T.pack (show exitCode)
          update
            key
            [ M.VmStatus =. M.VmError
            , M.VmHealthcheck =. Nothing
            , M.VmSpicePort =. Nothing
            , M.VmErrorMessage =. Just msg
            , M.VmLastErrorAt =. Just now
            ]
    _ -> pure ()

-- | Update the healthcheck timestamp on the VM.
updateHealthcheck :: Int64 -> UTCTime -> SqlPersistT IO ()
updateHealthcheck vmId now =
  update (M.toSqlKey vmId :: M.VmId) [M.VmHealthcheck =. Just now]

-- | Update guest IP addresses on network interfaces by matching MAC
-- addresses. Clears guest data on interfaces that don't match any
-- guest-reported interface (so stale data drops as soon as the
-- guest's view of the world changes).
updateGuestNetworkData
  :: Int64 -> [C.Parsed CGNA.GuestNetIf] -> SqlPersistT IO ()
updateGuestNetworkData vmId guestIfs = do
  let vmKey = M.toSqlKey vmId :: M.VmId
  hostIfs <- selectList [M.NetworkInterfaceVmId ==. vmKey] []
  forM_ hostIfs $ \(Entity ifKey hostIf) -> do
    let hostMac = T.toLower (M.networkInterfaceMacAddress hostIf)
        mGuestIf =
          find (\g -> T.toLower (CGNA.hwAddress g) == hostMac) guestIfs
    case mGuestIf of
      Nothing ->
        update ifKey [M.NetworkInterfaceGuestIpAddresses =. Nothing]
      Just guestIf ->
        let ips = CGNA.ipAddresses guestIf
            value =
              if null ips
                then Nothing
                else Just (formatIpAddresses ips)
         in update ifKey [M.NetworkInterfaceGuestIpAddresses =. value]

-- | Format @[GuestIpAddress]@ as "addr/prefix,addr/prefix".
formatIpAddresses :: [C.Parsed CGNA.GuestIpAddress] -> Text
formatIpAddresses =
  T.intercalate ","
    . map (\ip -> CGNA.ipAddress ip <> "/" <> tshow (CGNA.prefix ip))

-- ---------------------------------------------------------------------------
-- VM stats ring + subscriber fanout

-- | Append a 'VmStats' sample to the per-VM ring buffer, trimming
-- to 'vmStatsRingCapacity' from the left (oldest-first) so the
-- newest sample is always at the right.
appendVmStats :: ServerState -> Int64 -> C.Parsed CGVm.VmStats -> IO ()
appendVmStats state vmId stats =
  atomically $
    modifyTVar' (ssVmStatsRing state) $
      Map.alter (Just . trim . maybe (Seq.singleton stats) (Seq.|> stats)) vmId
  where
    trim s =
      let n = Seq.length s
       in if n > vmStatsRingCapacity
            then Seq.drop (n - vmStatsRingCapacity) s
            else s

-- | Forget the ring buffer for a VM that just transitioned out of
-- running. Subscribers stay; they receive the zero-filled
-- 'VmStats' from 'pushVmStats' so they can render "stopped" too.
dropVmStatsRing :: ServerState -> Int64 -> IO ()
dropVmStatsRing state vmId =
  atomically $ modifyTVar' (ssVmStatsRing state) (Map.delete vmId)

-- | Push a 'VmStats' to every 'vm.subscribeStats' subscriber
-- registered for this VM. Dead sinks (push raises OR no ACK
-- within 'subscriberPushTimeoutMicros') are pruned from the
-- registry on the spot. Mirrors 'pushGuestAgentStatus'.
--
-- The timeout matters: 'pushVmStats' runs synchronously inside
-- the agent → daemon 'vmStatusSink'onSnapshot' handler (which
-- itself uses 'handleParsed' — synchronous), so a 'callSink' that
-- blocks because some downstream client's RPC channel is wedged
-- would back-pressure the entire snapshot path all the way back
-- to the agent's ticker, freezing the per-VM healthcheck stamp.
pushVmStats :: ServerState -> Int64 -> C.Parsed CGVm.VmStats -> IO ()
pushVmStats state vmId stats = do
  subs <- readTVarIO (ssVmStatsSubs state)
  let sinks = Map.findWithDefault [] vmId subs
  case sinks of
    [] -> pure ()
    _ -> do
      let params = CGVm.VmStatsSink'onStats'params {CGVm.stats = stats}
      alive <- traverse (tryPush params) sinks
      atomically $
        modifyTVar' (ssVmStatsSubs state) $
          Map.insert vmId (map fst (filter snd (zip sinks alive)))
  where
    tryPush
      :: C.Parsed CGVm.VmStatsSink'onStats'params
      -> C.Client CGVm.VmStatsSink
      -> IO Bool
    tryPush params sink = do
      r <-
        E.try (timeout subscriberPushTimeoutMicros (callSink #onStats params sink))
          :: IO (Either E.SomeException (Maybe ()))
      pure $ case r of
        Right (Just ()) -> True
        _ -> False

-- ---------------------------------------------------------------------------

-- | Push a 'GuestAgentStatus' to every @vm.subscribeGuestAgent@
-- subscriber registered for this VM. Subscribers whose @push@
-- call raises are dropped from the registry, keeping the set
-- self-pruning. Mirrors the daemon's old in-process poller helper.
pushGuestAgentStatus :: ServerState -> Int64 -> Bool -> Int64 -> IO ()
pushGuestAgentStatus state vmId reachable pingMillis = do
  subs <- readTVarIO (ssGuestAgentSubs state)
  let sinks = Map.findWithDefault [] vmId subs
  case sinks of
    [] -> pure ()
    _ -> do
      mVm <- runSqlPool (get (M.toSqlKey vmId :: M.VmId)) (ssDbPool state)
      now <- getCurrentTime
      let nanos t =
            floor (nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds t) * 1e9)
              :: Int64
          lastHc
            | reachable && pingMillis > 0 =
                pingMillis * 1000000
            | reachable =
                nanos now
            | otherwise =
                maybe 0 nanos (M.vmHealthcheck =<< mVm)
          status :: C.Parsed CGS.GuestAgentStatus
          status =
            CGS.GuestAgentStatus
              { CGS.vmId = vmId
              , CGS.lastHealthcheck = lastHc
              , CGS.enabled = maybe False M.vmGuestAgent mVm
              , CGS.reachable = reachable
              , CGS.message = T.empty
              }
          params = CGS.GuestAgentStatusSink'push'params {CGS.status = status}
      alive <- traverse (tryPush params) sinks
      atomically $
        modifyTVar' (ssGuestAgentSubs state) $
          Map.insert vmId (map fst (filter snd (zip sinks alive)))
  where
    tryPush
      :: C.Parsed CGS.GuestAgentStatusSink'push'params
      -> C.Client CGS.GuestAgentStatusSink
      -> IO Bool
    tryPush params sink = do
      r <-
        E.try (timeout subscriberPushTimeoutMicros (callSink #push params sink))
          :: IO (Either E.SomeException (Maybe ()))
      pure $ case r of
        Right (Just ()) -> True
        _ -> False

-- | Per-push deadline for daemon → client sink fanout
-- ('pushVmStats' / 'pushGuestAgentStatus'). 5 s mirrors the
-- agent-side 'dispatchTimeoutMicros'; the daemon's snapshot
-- handler runs synchronously inside the agent's ticker chain, so
-- a wedged subscriber that exceeds this budget is treated as
-- dead and pruned. The frontend / WebUI will re-subscribe on its
-- next reconnect.
subscriberPushTimeoutMicros :: Int
subscriberPushTimeoutMicros = 5000000

-- | Stamp a 'NodeStats' observation into the 'Node' row that
-- corresponds to the agent connection this sink is bound to.
-- Bumps 'nodeAgentHealthcheck' to the agent's snapshot wall
-- clock so the scheduler can filter on freshness. Clears the
-- scheduler's pending RAM reservation for this node so we
-- don't double-count headroom that the agent's fresh
-- @ramMbFree@ already reflects.
applyNodeStats
  :: ServerState
  -> M.NodeId
  -> C.Parsed CGNA.NodeStats
  -> Int64
  -- ^ snapshot millis (agent wall clock)
  -> IO ()
applyNodeStats state nid stats snapMs = do
  let CGNA.NodeStats
        { CGNA.cpuCount = cpus
        , CGNA.ramMbTotal = ramTotal
        , CGNA.ramMbFree = ramFree
        , CGNA.storageBytesTotal = storTotal
        , CGNA.storageBytesFree = storFree
        , CGNA.loadAvg1 = l1
        , CGNA.loadAvg5 = l5
        , CGNA.loadAvg15 = l15
        , CGNA.kernelRelease = kernel
        , CGNA.agentVersion = ver
        } = stats
      observedAt = millisToUtc snapMs
      maybeIfNonZero x = if x == 0 then Nothing else Just (fromIntegral x)
      maybeIfNonZeroD x = if x == 0 then Nothing else Just x
      maybeText t = if T.null t then Nothing else Just t
  -- Capture the previously-recorded agent version BEFORE the
  -- update so 'refuseMismatchedAgent' can tell a brand-new
  -- mismatch (agent rebuilt to a different but still-skewed
  -- hash) from an already-known one (operator just flipped
  -- admin state back to 'online' without rebuilding). Without
  -- this guard the auto-drain would silently re-fire on every
  -- ~10 s push and undo the manual edit.
  prevAgentVersion <- do
    mPrev <- runSqlPool (get nid) (ssDbPool state)
    pure $ mPrev >>= M.nodeAgentVersion
  runSqlPool
    ( update
        nid
        [ M.NodeCpuCount =. maybeIfNonZero cpus
        , M.NodeRamMbTotal =. maybeIfNonZero ramTotal
        , M.NodeRamMbFree =. maybeIfNonZero ramFree
        , M.NodeStorageBytesTotal =. maybeIfNonZero64 storTotal
        , M.NodeStorageBytesFree =. maybeIfNonZero64 storFree
        , M.NodeLoadAvg1 =. maybeIfNonZeroD l1
        , M.NodeLoadAvg5 =. maybeIfNonZeroD l5
        , M.NodeLoadAvg15 =. maybeIfNonZeroD l15
        , M.NodeKernelRelease =. maybeText kernel
        , M.NodeAgentVersion =. maybeText ver
        , M.NodeNodeAgentHealthcheck =. Just observedAt
        ]
    )
    (ssDbPool state)
  -- See note above re: re-counting.
  Corvus.Types.clearReservation state nid
  -- Refuse to schedule onto a node whose agent build differs
  -- from the daemon's. The agent reports its short git hash in
  -- 'NodeStats.agentVersion'; the daemon was built from the
  -- same library, so 'NS.agentVersion' is the local truth. On
  -- mismatch we flip the node to 'NodeDraining' so the
  -- scheduler skips it but existing VMs / network state survive
  -- — and log loudly so the operator notices. Only fire on a
  -- NEW mismatch (previous version differs from current) so an
  -- operator's manual @crv node edit --admin-state online@ is
  -- sticky across subsequent pushes from the same agent build.
  when
    ( not (T.null ver)
        && ver /= NS.agentVersion
        && prevAgentVersion /= Just ver
    )
    $ refuseMismatchedAgent state nid ver
  where
    maybeIfNonZero64 :: Int64 -> Maybe Int
    maybeIfNonZero64 x = if x == 0 then Nothing else Just (fromIntegral x)

-- | Flip a mismatched-agent node to 'NodeDraining' on a newly-
-- observed agent version that differs from the daemon. The
-- caller ('applyNodeStats') gates this on
-- @prevAgentVersion \/= Just ver@, so this only fires when the
-- agent build changes (first push, or a rebuild that still
-- doesn't match). A manual @crv node edit --admin-state online@
-- without an agent rebuild leaves the stored version equal to
-- the current snapshot's version, so the gate skips the call
-- and the operator's intent sticks.
refuseMismatchedAgent :: ServerState -> M.NodeId -> Text -> IO ()
refuseMismatchedAgent state nid agentVer = do
  mNode <- runSqlPool (get nid) (ssDbPool state)
  case mNode of
    Just n | M.nodeAdminState n == M.NodeOnline -> do
      runSqlPool
        (update nid [M.NodeAdminState =. M.NodeDraining])
        (ssDbPool state)
      runServerLogging state $
        logWarnN $
          "node "
            <> M.nodeName n
            <> " agent version "
            <> agentVer
            <> " does not match daemon "
            <> NS.agentVersion
            <> "; marking draining so the scheduler skips it"
    _ -> pure ()

millisToUtc :: Int64 -> UTCTime
millisToUtc ms =
  posixSecondsToUTCTime
    (realToFrac (fromIntegral ms / 1000 :: Double))

tshow :: (Show a) => a -> Text
tshow = T.pack . show
