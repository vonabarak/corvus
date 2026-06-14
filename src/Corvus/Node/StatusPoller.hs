{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  , dispatchVm
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Vm as CGVM
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  , writeTVar
  )
import qualified Control.Exception as E
import Control.Monad (foldM, forever)
import Control.Monad.Logger (logWarnN, runStderrLoggingT)
import qualified Corvus.Node.GuestAgent as NGA
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.NodeStats as NS
import qualified Corvus.Node.ProcStats as PS
import qualified Corvus.Node.Qmp as Qmp
import qualified Corvus.Node.VmSpec as VS
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Rpc.Streams (callSink)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
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
  { subsList :: TVar [C.Client CGNA.VmStatusSink]
  , subsPriorSample :: IORef (Map.Map Int64 POSIXTime)
  -- ^ Wall-clock of the previous successful stats sample per VM.
  -- @intervalMillis = now - prior@. First sample for a VM has
  -- @prior == Nothing@ so we emit @intervalMillis = 0@; consumers
  -- treat that as "no rate yet".
  }

newSubscribers :: IO Subscribers
newSubscribers = Subscribers <$> newTVarIO [] <*> newIORef Map.empty

addSubscriber :: Subscribers -> C.Client CGNA.VmStatusSink -> IO ()
addSubscriber Subscribers {subsList = ref} sink =
  atomically $ modifyTVar' ref (sink :)

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
    now <- millisNow
    -- Build entries in parallel: one stuck guest agent (typically
    -- a VM whose QGA MVar is held by a long-running vmGuestExec
    -- from an in-flight build) would otherwise block the entire
    -- tick via 'mapM', starving every other VM's status push and
    -- leaving all of them with stale 'healthcheck' timestamps
    -- ('crv vm list' shows them all as DOWN). Per-VM QGA
    -- serialisation still applies inside 'buildEntry' for the
    -- single stuck VM; the rest proceed independently.
    entries <-
      Async.forConcurrently
        (Map.toList vms)
        (buildEntry cfg qgaConns subs clk)
    -- Per-tick node observation: CPU/RAM/disk/load/kernel/version.
    -- The daemon-side sink stamps these into the 'Node' row and
    -- bumps 'nodeAgentHealthcheck'.
    stats <- NS.readNodeStats cfg
    let snapshot =
          CGNA.VmStatusSnapshot
            { CGNA.snapshotAtMillis = now
            , CGNA.entries = entries
            , CGNA.nodeStats = stats
            }
    dispatch subs snapshot

-- | Push a single-VM snapshot to every subscriber, out of band
-- from the 10 s ticker. Used by the agent's @vmStart@ forked
-- post-spawn watcher to signal "first QGA ping landed" (or
-- "QGA never came up, VM torn down") to the daemon without
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
      entry <- buildEntry cfg qgaConns subs clk (vmId, live)
      now <- millisNow
      stats <- NS.readNodeStats cfg
      let snapshot =
            CGNA.VmStatusSnapshot
              { CGNA.snapshotAtMillis = now
              , CGNA.entries = [entry]
              , CGNA.nodeStats = stats
              }
      dispatch subs snapshot

-- | One pass per VM in the ledger. Reads the reaper's last-exit
-- code, decides the agent-side state, and (for running VMs)
-- pings QGA + fetches network interfaces + samples resource
-- counters.
buildEntry
  :: QemuConfig
  -> NGA.GuestAgentConns
  -> Subscribers
  -> Word32
  -- ^ _SC_CLK_TCK, captured once per ticker for stamping on every
  -- 'VmStats' sample so consumers can convert jiffies to seconds.
  -> (Int64, L.VmLiveState)
  -> IO (C.Parsed CGNA.VmStatusEntry)
buildEntry cfg qgaConns subs clk (vmId, live) = do
  mExit <- readTVarIO (L.vlsLastExitCode live)
  let qpid = fromIntegral (L.vlsQemuPid live) :: Int32
  case mExit of
    Just 0 -> pure $ baseEntry vmId CGNA.VmAgentState'stopped qpid 0 False 0 [] (zeroVmStats clk)
    Just code ->
      pure $
        baseEntry
          vmId
          CGNA.VmAgentState'errored
          qpid
          (fromIntegral code)
          False
          0
          []
          (zeroVmStats clk)
    Nothing -> do
      -- Still running; probe QGA.
      pingResult <- E.try @E.SomeException (NGA.guestPing qgaConns cfg vmId)
      let ok = case pingResult of
            Right b -> b
            Left _ -> False
      pingedAt <- if ok then millisNow else pure 0
      ifs <-
        if ok
          then do
            r <- E.try @E.SomeException (NGA.guestNetworkGetInterfaces qgaConns cfg vmId)
            pure $ case r of
              Right (Just list) -> list
              _ -> []
          else pure []
      let wireIfs = map encodeIf ifs
      stats <- sampleVmStats cfg subs clk vmId live
      pure $
        baseEntry vmId CGNA.VmAgentState'running qpid 0 ok pingedAt wireIfs stats

baseEntry
  :: Int64
  -> CGNA.VmAgentState
  -> Int32
  -> Int32
  -> Bool
  -> Int64
  -> [C.Parsed CGNA.GuestNetIf]
  -> C.Parsed CGVM.VmStats
  -> C.Parsed CGNA.VmStatusEntry
baseEntry vid st qpid code ok pingedAt ifs stats =
  CGNA.VmStatusEntry
    { CGNA.vmId = vid
    , CGNA.state = st
    , CGNA.qemuPid = qpid
    , CGNA.lastExitCode = code
    , CGNA.guestAgentOk = ok
    , CGNA.lastPingMillis = pingedAt
    , CGNA.netIfs = ifs
    , CGNA.stats = stats
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
    E.handle (\(_ :: E.SomeException) -> pure Nothing) $
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
      balloonMax = fromIntegral (VS.vsRamMb spec) * 1024 * 1024 :: Word64
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

-- | Wrap a QMP-returning IO in a try + 'Either'-flatten so the
-- caller always sees @Nothing@ on any failure path.
safeQmpResult :: IO (Either e a) -> IO (Maybe a)
safeQmpResult io =
  E.handle (\(_ :: E.SomeException) -> pure Nothing) $
    either (const Nothing) Just <$> io

sampleTap :: T.Text -> IO (Maybe (C.Parsed CGVM.NetIo))
sampleTap tap = do
  m <-
    E.handle (\(_ :: E.SomeException) -> pure Nothing) $
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
dispatch :: Subscribers -> C.Parsed CGNA.VmStatusSnapshot -> IO ()
dispatch Subscribers {subsList = ref} snapshot = do
  sinks <- readTVarIO ref
  remaining <-
    foldM
      ( \acc sink -> do
          r <-
            E.try @E.SomeException $
              timeout dispatchTimeoutMicros (deliver sink)
          case r of
            Right (Just ()) -> pure (sink : acc)
            _ -> do
              let reason = case r of
                    Left e -> T.pack (show e)
                    Right Nothing -> "no ACK within 5s"
                    Right (Just ()) -> "" -- unreachable
              runStderrLoggingT $
                logWarnN
                  ("[nodeagent] dropping VmStatusSink: " <> reason)
              pure acc
      )
      []
      sinks
  atomically $ writeTVar ref (reverse remaining)
  where
    deliver =
      callSink
        #onSnapshot
        CGNA.VmStatusSink'onSnapshot'params {CGNA.snapshot = snapshot}

-- | Per-push deadline for 'dispatch'. Bounded by the 10 s tick
-- interval — a sink that can't ACK within 5 s is treated as
-- dead, dropped from the registry, and skipped on subsequent
-- ticks. The daemon dials the agent and re-subscribes on its
-- next reconnect attempt.
dispatchTimeoutMicros :: Int
dispatchTimeoutMicros = 5000000

millisNow :: IO Int64
millisNow = round . (* 1000) <$> getPOSIXTime
