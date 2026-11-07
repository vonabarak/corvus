{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Periodic status push for `corvus-nodeagent`.
--
-- Once a daemon has registered a 'VmStatusSink' via
-- 'subscribeVmStatus', the agent runs one ticker thread per
-- process that fires every ~10 s. On each tick:
--
--   * walk the 'VmLedger' and read each VM's current state
--     (running / stopped / errored, via 'vlsLastExitCode');
--   * for VMs still running, probe QGA with 'guestPing' (short
--     timeout) and on success fetch
--     'guestNetworkGetInterfaces';
--   * build one 'VmStatusSnapshot' covering every entry and
--     dispatch it to every subscriber;
--   * prune any sink that throws on dispatch (the daemon went
--     away). Same pattern netd uses for
--     'Corvus.Netd.Events.dispatchVanished'.
--
-- This is the slice C replacement for the daemon-side per-VM
-- 'startGuestAgentPoller' threads.
module Corvus.Node.StatusPoller
  ( Subscribers
  , newSubscribers
  , addSubscriber
  , runStatusPoller
  , recordGuestAgentReady
  , dispatchVm
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Vm as CGVM
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import qualified Control.Exception as E
import Control.Monad (forever, when)
import Control.Monad.Logger (LogLevel (..))
import Corvus.Logging (exceptionCategory, subscriberFailureLevel, synchronousException)
import qualified Corvus.Node.GuestAgent as NGA
import Corvus.Node.GuestAgent.Connection (GuestAgentConns (..))
import Corvus.Node.GuestAgent.Transport (logQgaEvent)
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.NodeStats as NS
import qualified Corvus.Node.ProcStats as PS
import qualified Corvus.Node.Qmp as Qmp
import Corvus.Node.StatusSubscribers (SubscriberRegistry, dispatchSubscribers, newSubscriberRegistry, registerSubscriber)
import Corvus.Node.StorageStats (StorageStatsReader, newStorageStatsReader)
import qualified Corvus.Node.VmSpec as VS
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Rpc.Streams (callSink)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Word (Word32, Word64)
import System.Timeout (timeout)

-- | Process-wide registry of 'VmStatusSink' caps + the
-- per-VM prior-sample timestamp ('subsPriorSample') used to fill
-- 'VmStats.intervalMillis'. Sink subscribers are appended on
-- 'subscribeVmStatus'; the ticker prunes dead ones on dispatch
-- failure.
data Subscribers = Subscribers
  { subsList :: SubscriberRegistry (C.Client CGNA.VmStatusSink)
  , subsGuestHealth :: TVar (Map.Map (Int64, Int64, Int64, Word32) (Bool, Int64, [C.Parsed CGNA.GuestNetIf]))
  , subsStorage :: StorageStatsReader
  , subsPriorSample :: IORef (Map.Map Int64 POSIXTime)
  -- ^ Wall-clock of the previous successful stats sample per VM.
  -- @intervalMillis = now - prior@. First sample for a VM has
  -- @prior == Nothing@ so we emit @intervalMillis = 0@; consumers
  -- treat that as "no rate yet".
  }

newSubscribers :: IO Subscribers
newSubscribers = Subscribers <$> newSubscriberRegistry <*> newTVarIO Map.empty <*> newStorageStatsReader <*> newIORef Map.empty

addSubscriber :: Subscribers -> C.Client CGNA.VmStatusSink -> IO ()
addSubscriber Subscribers {subsList = ref} = registerSubscriber ref

-- | Run the periodic snapshot pusher. Blocks forever; spawn as
-- a long-lived async from 'runNodeAgentServer'.
--
-- @tickIntervalMs@ controls the cadence. 10 s is the default
-- the daemon expects; tests pass a smaller value.
runStatusPoller
  :: QemuConfig
  -> L.VmLedger
  -> NGA.GuestAgentConns
  -> Subscribers
  -> Int
  -- ^ Tick interval in milliseconds.
  -> IO ()
runStatusPoller cfg ledger qgaConns subs tickIntervalMs = do
  clk <- PS.getClkTck
  forever $ do
    threadDelay (tickIntervalMs * 1000)
    vms <- atomically $ L.readVms ledger
    atomically $ do
      observations <- readTVar (subsGuestHealth subs)
      writeTVar (subsGuestHealth subs) (Map.filterWithKey (\(vid, rev, gen, pid) _ -> maybe False (\live -> VS.vsLifecycleRevision (L.vlsSpec live) == rev && VS.vsRuntimeGeneration (L.vlsSpec live) == gen && L.vlsQemuPid live == pid) (Map.lookup vid vms)) observations)
    now <- millisNow
    -- Sample VMs concurrently, with bounded QGA observations in buildEntry:
    -- the snapshot cannot be dispatched until every entry has completed.
    entries <-
      Async.forConcurrently
        (Map.toList vms)
        (buildEntry ledger "periodic" cfg qgaConns subs clk)
    -- Per-tick node observation: CPU/RAM/disk/load/kernel/version.
    -- The daemon-side sink stamps these into the 'Node' row and
    -- bumps 'nodeAgentHealthcheck'.
    stats <- NS.readNodeStats (subsStorage subs) (guestAgentLogger qgaConns) cfg
    published <- mapM (validateObservation ledger clk) (catMaybes entries)
    let snapshot =
          CGNA.VmStatusSnapshot
            { CGNA.snapshotAtMillis = now
            , CGNA.entries = catMaybes published
            , CGNA.nodeStats = stats
            }
    dispatch qgaConns subs snapshot

-- | Push a single-VM snapshot to every subscriber, out of band
-- from the 10 s ticker. Used by the agent's @vmStart@ forked
-- post-spawn watcher to signal "first QGA ping landed" or
-- "first QGA ping timed out" to the daemon without
-- waiting for the next tick.
--
-- The entry is built from the same path the periodic poller
-- uses, so the daemon-side 'VmStatusSink' handler can't tell an
-- on-demand push apart from a regular tick. If the VM isn't in
-- the ledger anymore (e.g. removed before this call landed), the
-- push is a no-op.
dispatchVm
  :: QemuConfig
  -> NGA.GuestAgentConns
  -> L.VmLedger
  -> Subscribers
  -> Int64
  -> IO ()
dispatchVm cfg qgaConns ledger subs vmId = do
  vms <- atomically $ L.readVms ledger
  case Map.lookup vmId vms of
    Nothing -> pure ()
    Just live -> do
      clk <- PS.getClkTck
      entry <- buildEntry ledger "dispatch" cfg qgaConns subs clk (vmId, live)
      now <- millisNow
      stats <- NS.readNodeStats (subsStorage subs) (guestAgentLogger qgaConns) cfg
      published <- mapM (validateObservation ledger clk) (catMaybes [entry])
      let snapshot =
            CGNA.VmStatusSnapshot
              { CGNA.snapshotAtMillis = now
              , CGNA.entries = catMaybes published
              , CGNA.nodeStats = stats
              }
      dispatch qgaConns subs snapshot

-- | One pass per VM in the ledger. Reads the reaper's last-exit
-- code, decides the agent-side state, and (for running VMs)
-- pings QGA + fetches network interfaces + samples resource
-- counters.
buildEntry
  :: L.VmLedger
  -> T.Text
  -> QemuConfig
  -> NGA.GuestAgentConns
  -> Subscribers
  -> Word32
  -- ^ _SC_CLK_TCK, captured once per ticker for stamping on every
  -- 'VmStats' sample so consumers can convert jiffies to seconds.
  -> (Int64, L.VmLiveState)
  -> IO (Maybe (L.VmLiveState, (Int, Integer), C.Parsed CGNA.VmStatusEntry))
buildEntry ledger origin cfg qgaConns subs clk (vmId, live) = do
  before <- readTVarIO (L.vlsStopState live)
  entry <- observe before
  pure $ Just (live, before, entry)
  where
    observe before = do
      mExit <- readTVarIO (L.vlsLastExitCode live)
      (startupError, startupPending) <-
        atomically $
          (,) <$> readTVar (L.vlsStartupError live) <*> readTVar (L.vlsStartupPending live)
      let qpid = fromIntegral (L.vlsQemuPid live) :: Int32
      case mExit of
        Just 0 -> pure $ baseEntry vmId live CGNA.VmAgentState'stopped qpid 0 False 0 [] (zeroVmStats clk) startupError
        Just code ->
          pure $
            baseEntry
              vmId
              live
              CGNA.VmAgentState'errored
              qpid
              (fromIntegral code)
              False
              0
              []
              (zeroVmStats clk)
              startupError
        Nothing -> do
          let spec = L.vlsSpec live
              fields =
                "vm_id="
                  <> shown vmId
                  <> " origin="
                  <> origin
                  <> " runtime_generation="
                  <> shown (VS.vsRuntimeGeneration spec)
                  <> " lifecycle_revision="
                  <> shown (VS.vsLifecycleRevision spec)
                  <> " qemu_pid="
                  <> shown qpid
                  <> " startup_pending="
                  <> shown startupPending
                  <> " startup_error_present="
                  <> shown (isJust startupError)
                  <> " startup_budget_ms="
                  <> shown (VS.vsWaitForGuestAgentMs spec)
                  <> " start_paused="
                  <> shown (VS.vsStartPaused spec)
                  <> " timeout_us=5000000"
              event level name extra = logQgaEvent (guestAgentLogger qgaConns) level name (fields <> " " <> extra)
          let enabled = VS.vsGuestAgent spec
              probe = enabled && not startupPending && isNothing startupError && fst before == 0
              identity = (vmId, VS.vsLifecycleRevision spec, VS.vsRuntimeGeneration spec, L.vlsQemuPid live)
          event LevelDebug "status_probe_decision" ("probe=" <> shown probe)
          result <- if probe then NGA.guestHealthProbeWhen (eligible before) qgaConns cfg vmId else pure (Just (Left "Guest agent probe skipped"))
          unchanged <- eligible before
          cached <- Map.findWithDefault (False, 0, []) identity <$> readTVarIO (subsGuestHealth subs)
          now <- millisNow
          let (ok, pingedAt, wireIfs, deferred, valid, message) = case if unchanged then result else Nothing of
                Nothing -> let (priorOk, priorTime, priorIfs) = cached in (priorOk, priorTime, priorIfs, True, False, "Guest agent busy; observation deferred")
                Just (Left reason) -> (False, 0, let (_, _, priorIfs) = cached in priorIfs, False, False, reason)
                Just (Right interfaces) -> (True, now, maybe (let (_, _, priorIfs) = cached in priorIfs) (map encodeIf) interfaces, False, isJust interfaces, T.empty)
          stats <- sampleVmStats cfg subs clk vmId live
          committed <- atomically $ do
            current <- eligibleSTM before
            when (probe && current && not deferred) $ do
              observations <- readTVar (subsGuestHealth subs)
              writeTVar (subsGuestHealth subs) (Map.insert identity (ok, pingedAt, wireIfs) observations)
            pure current
          when (probe && committed && not deferred && not ok) $
            event LevelWarn "status_probe_failed" "Guest agent health observation failed"
          event LevelDebug "status_ping_end" ("deferred=" <> shown (deferred || not committed) <> " reachable=" <> shown ok)
          pure $
            (baseEntry vmId live CGNA.VmAgentState'running qpid 0 ok pingedAt wireIfs stats startupError)
              { CGNA.guestAgentDeferred = deferred || not committed
              , CGNA.guestAgentMessage = if enabled then message else "Guest agent disabled"
              , CGNA.netIfsValid = valid && committed
              }

    eligible before = atomically (eligibleSTM before)
    eligibleSTM before = do
      current <- L.lookupVm ledger vmId
      stop <- readTVar (L.vlsStopState live)
      exited <- readTVar (L.vlsLastExitCode live)
      pending <- readTVar (L.vlsStartupPending live)
      failure <- readTVar (L.vlsStartupError live)
      pure $ maybe False (`sameRuntime` live) current && stop == before && fst stop == 0 && isNothing exited && not pending && isNothing failure

-- Validate again after every VM and node measurement has completed.
validateObservation :: L.VmLedger -> Word32 -> (L.VmLiveState, (Int, Integer), C.Parsed CGNA.VmStatusEntry) -> IO (Maybe (C.Parsed CGNA.VmStatusEntry))
validateObservation ledger clk (live, before, entry) = atomically $ do
  let vmId = VS.vsVmId (L.vlsSpec live)
  current <- L.lookupVm ledger vmId
  after <- readTVar (L.vlsStopState live)
  exited <- readTVar (L.vlsLastExitCode live)
  pure $ case current of
    Just runtime | sameRuntime runtime live ->
      Just $ case exited of
        Just code -> baseEntry vmId live (if code == 0 then CGNA.VmAgentState'stopped else CGNA.VmAgentState'errored) (fromIntegral (L.vlsQemuPid live)) (fromIntegral code) False 0 [] (zeroVmStats clk) (Just (CGNA.startupError entry))
        Nothing ->
          if before /= after || fst after > 0
            then entry {CGNA.guestAgentDeferred = True, CGNA.netIfsValid = False, CGNA.guestAgentMessage = "Runtime lifecycle changed; observation deferred"}
            else entry
    _ -> Nothing

sameRuntime :: L.VmLiveState -> L.VmLiveState -> Bool
sameRuntime a b = L.vlsQemuPid a == L.vlsQemuPid b && VS.vsRuntimeGeneration (L.vlsSpec a) == VS.vsRuntimeGeneration (L.vlsSpec b) && VS.vsLifecycleRevision (L.vlsSpec a) == VS.vsLifecycleRevision (L.vlsSpec b)

-- | Seed readiness before exposing the runtime to nonblocking health probes.
recordGuestAgentReady :: Subscribers -> L.VmLiveState -> IO ()
recordGuestAgentReady subs live = do
  now <- millisNow
  let spec = L.vlsSpec live
      identity = (VS.vsVmId spec, VS.vsLifecycleRevision spec, VS.vsRuntimeGeneration spec, L.vlsQemuPid live)
  atomically $ do
    observations <- readTVar (subsGuestHealth subs)
    writeTVar (subsGuestHealth subs) (Map.insert identity (True, now, []) observations)

baseEntry
  :: Int64
  -> L.VmLiveState
  -> CGNA.VmAgentState
  -> Int32
  -> Int32
  -> Bool
  -> Int64
  -> [C.Parsed CGNA.GuestNetIf]
  -> C.Parsed CGVM.VmStats
  -> Maybe T.Text
  -> C.Parsed CGNA.VmStatusEntry
baseEntry vid live st qpid code ok pingedAt ifs stats startupError =
  CGNA.VmStatusEntry
    { CGNA.vmId = vid
    , CGNA.state = st
    , CGNA.qemuPid = qpid
    , CGNA.lastExitCode = code
    , CGNA.guestAgentOk = ok
    , CGNA.lastPingMillis = pingedAt
    , CGNA.netIfs = ifs
    , CGNA.stats = stats
    , CGNA.lifecycleRevision = VS.vsLifecycleRevision (L.vlsSpec live)
    , CGNA.runtimeGeneration = VS.vsRuntimeGeneration (L.vlsSpec live)
    , CGNA.guestAgentDeferred = False
    , CGNA.guestAgentMessage = T.empty
    , CGNA.netIfsValid = False
    , CGNA.startupError = fromMaybe T.empty startupError
    }

-- ---------------------------------------------------------------------------
-- Stats sampling

-- | Zero-filled 'VmStats' used when the VM is not running. The
-- clk-tck field is still stamped so consumers don't have to
-- special-case stopped VMs.
zeroVmStats :: Word32 -> C.Parsed CGVM.VmStats
zeroVmStats clk =
  CGVM.VmStats
    { CGVM.sampledAtNanos = 0
    , CGVM.intervalMillis = 0
    , CGVM.cpuJiffiesTotal = 0
    , CGVM.clkTck = clk
    , CGVM.hostRssBytes = 0
    , CGVM.balloonActualBytes = 0
    , CGVM.balloonMaxBytes = 0
    , CGVM.drives = []
    , CGVM.nets = []
    }

-- | Take one resource-usage sample for a running VM. Each
-- per-source read is wrapped in a 'try' so a transient failure
-- (process exited mid-sample, QMP socket unwritable, balloon
-- device hot-unplugged) just degrades the affected field to
-- zero rather than dropping the entire entry.
sampleVmStats
  :: QemuConfig
  -> Subscribers
  -> Word32
  -> Int64
  -> L.VmLiveState
  -> IO (C.Parsed CGVM.VmStats)
sampleVmStats cfg subs clk vmId live = do
  now <- getPOSIXTime
  prior <-
    atomicModifyIORef'
      (subsPriorSample subs)
      (\m -> (Map.insert vmId now m, Map.lookup vmId m))
  let intervalMs = case prior of
        Nothing -> 0
        Just p -> round ((now - p) * 1000) :: Word32

  mProc <-
    E.handleJust synchronousException (\_ -> pure Nothing) $
      PS.readProcSample (L.vlsQemuPid live)
  let (jiff, rss) = case mProc of
        Just s -> (PS.psCpuJiffies s, PS.psRssBytes s)
        Nothing -> (0, 0)

  blockstats <- safeQmpResult (Qmp.qmpQueryBlockstats cfg vmId)
  balloonActualMaybe <- safeQmpResult (Qmp.qmpQueryBalloon cfg vmId)
  let balloonActual = case balloonActualMaybe of
        Just (Just bytes) -> bytes
        _ -> 0

  let spec = L.vlsSpec live
      balloonMax = if VS.vsBalloon spec then fromIntegral (VS.vsRam spec) else 0 :: Word64
      tapNames = map VS.vnsHostDevice (VS.vsNetIfs spec)
  tapEntries <- mapM sampleTap tapNames

  pure
    CGVM.VmStats
      { CGVM.sampledAtNanos = floor (now * 1e9)
      , CGVM.intervalMillis = intervalMs
      , CGVM.cpuJiffiesTotal = jiff
      , CGVM.clkTck = clk
      , CGVM.hostRssBytes = rss
      , CGVM.balloonActualBytes = balloonActual
      , CGVM.balloonMaxBytes = balloonMax
      , CGVM.drives = maybe [] (map encodeOneDrive) blockstats
      , CGVM.nets = catMaybes tapEntries
      }

-- | Resource counters are optional observations, not prerequisites for
-- reporting startup readiness. Bound each QMP exchange (including connect
-- and greeting) so a live but unresponsive monitor cannot stall either the
-- first-ping notification or the entire periodic snapshot.
safeQmpResult :: IO (Either e a) -> IO (Maybe a)
safeQmpResult io =
  E.handleJust synchronousException (\_ -> pure Nothing) $ do
    result <- timeout 1000000 io
    pure $ case result of
      Just (Right value) -> Just value
      _ -> Nothing

sampleTap :: T.Text -> IO (Maybe (C.Parsed CGVM.NetIo))
sampleTap tap = do
  m <-
    E.handleJust synchronousException (\_ -> pure Nothing) $
      PS.readTapSample tap
  pure $ case m of
    Nothing -> Nothing
    Just s ->
      Just
        CGVM.NetIo
          { CGVM.tapName = tap
          , CGVM.rxBytesTotal = PS.tsRxBytes s
          , CGVM.txBytesTotal = PS.tsTxBytes s
          }

encodeOneDrive :: Qmp.BlockstatsRow -> C.Parsed CGVM.DriveIo
encodeOneDrive row =
  CGVM.DriveIo
    { CGVM.name = Qmp.bsrDevice row
    , CGVM.readBytesTotal = Qmp.bsrRdBytes row
    , CGVM.writeBytesTotal = Qmp.bsrWrBytes row
    , CGVM.readOpsTotal = Qmp.bsrRdOps row
    , CGVM.writeOpsTotal = Qmp.bsrWrOps row
    }

-- Encoders from the agent-side NGA records to the wire structs.
-- Note: NGA.GuestNetIf doesn't carry an interface name (the
-- guest-network-get-interfaces QGA reply does, but our parser
-- dropped it long ago); leave 'CGNA.name' empty for now.
encodeIf :: NGA.GuestNetIf -> C.Parsed CGNA.GuestNetIf
encodeIf n =
  CGNA.GuestNetIf
    { CGNA.name = ""
    , CGNA.hwAddress = NGA.gniHardwareAddress n
    , CGNA.ipAddresses = map encodeIp (NGA.gniIpAddresses n)
    }

encodeIp :: NGA.GuestIpAddress -> C.Parsed CGNA.GuestIpAddress
encodeIp ip =
  CGNA.GuestIpAddress
    { CGNA.ipAddress = NGA.giaAddress ip
    , CGNA.prefix = fromIntegral (NGA.giaPrefix ip) :: Int32
    , CGNA.ipAddrType = NGA.giaType ip
    }

-- | Fire 'onSnapshot' on every registered sink. Sinks that throw
-- OR fail to ACK within 'dispatchTimeoutMicros' are pruned from
-- the registry so subsequent ticks don't keep hitting them — a
-- wedged downstream daemon (synchronous 'handleParsed' handler
-- back-pressured by its own stuck subscriber chain) would
-- otherwise freeze the ticker, since 'callSink' is a blocking
-- 'callP >>= waitPipeline' round-trip with no built-in deadline.
dispatch :: GuestAgentConns -> Subscribers -> C.Parsed CGNA.VmStatusSnapshot -> IO ()
dispatch conns Subscribers {subsList = ref} snapshot =
  dispatchSubscribers ref $ \sink -> do
    r <- E.tryJust synchronousException $ timeout dispatchTimeoutMicros (deliver sink)
    case r of
      Right (Just ()) -> pure True
      _ -> do
        let reason = case r of
              Left e -> exceptionCategory e
              Right Nothing -> "no ACK within 5s"
        guestAgentLogger
          conns
          (either subscriberFailureLevel (const LevelWarn) r)
          ("[nodeagent] dropping VmStatusSink: " <> reason)
        pure False
  where
    deliver =
      callSink
        #onSnapshot
        CGNA.VmStatusSink'onSnapshot'params {CGNA.snapshot = snapshot}

-- | Per-push deadline for 'dispatch'. Bounded by the 10 s tick
-- interval — a sink that can't ACK within 5 s is treated as
-- dead, dropped from the registry, and skipped on subsequent
-- ticks. The daemon dials the agent and re-subscribes on its
-- next reconnect attempt. Missing snapshots trigger that reconnect even when
-- the transport remains alive (45 s freshness deadline in NodeSupervisor).
dispatchTimeoutMicros :: Int
dispatchTimeoutMicros = 5000000

millisNow :: IO Int64
millisNow = round . (* 1000) <$> getPOSIXTime

shown :: (Show a) => a -> T.Text
shown = T.pack . show
