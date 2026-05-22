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
import Control.Concurrent (threadDelay)
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
import qualified Corvus.Node.VmSpec as VS
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Rpc.Streams (callSink)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)

-- | Process-wide registry of 'VmStatusSink' caps. Subscribers
-- are appended on 'subscribeVmStatus'; the ticker prunes dead
-- ones on dispatch failure.
newtype Subscribers = Subscribers
  { subsList :: TVar [C.Client CGNA.VmStatusSink]
  }

newSubscribers :: IO Subscribers
newSubscribers = Subscribers <$> newTVarIO []

addSubscriber :: Subscribers -> C.Client CGNA.VmStatusSink -> IO ()
addSubscriber (Subscribers ref) sink =
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
runStatusPoller cfg ledger qgaConns subs tickIntervalMs = forever $ do
  threadDelay (tickIntervalMs * 1000)
  vms <- atomically $ L.readVms ledger
  now <- millisNow
  entries <- mapM (buildEntry cfg qgaConns) (Map.toList vms)
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
      entry <- buildEntry cfg qgaConns (vmId, live)
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
-- pings QGA + fetches network interfaces.
buildEntry
  :: QemuConfig
  -> NGA.GuestAgentConns
  -> (Int64, L.VmLiveState)
  -> IO (C.Parsed CGNA.VmStatusEntry)
buildEntry cfg qgaConns (vmId, live) = do
  mExit <- readTVarIO (L.vlsLastExitCode live)
  let qpid = fromIntegral (L.vlsQemuPid live) :: Int32
  case mExit of
    Just 0 ->
      pure $
        baseEntry vmId CGNA.VmAgentState'stopped qpid 0 False 0 []
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
      pure $
        baseEntry vmId CGNA.VmAgentState'running qpid 0 ok pingedAt wireIfs

baseEntry
  :: Int64
  -> CGNA.VmAgentState
  -> Int32
  -> Int32
  -> Bool
  -> Int64
  -> [C.Parsed CGNA.GuestNetIf]
  -> C.Parsed CGNA.VmStatusEntry
baseEntry vid st qpid code ok pingedAt ifs =
  CGNA.VmStatusEntry
    { CGNA.vmId = vid
    , CGNA.state = st
    , CGNA.qemuPid = qpid
    , CGNA.lastExitCode = code
    , CGNA.guestAgentOk = ok
    , CGNA.lastPingMillis = pingedAt
    , CGNA.netIfs = ifs
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
-- are pruned from the registry so subsequent ticks don't keep
-- hitting them.
dispatch :: Subscribers -> C.Parsed CGNA.VmStatusSnapshot -> IO ()
dispatch (Subscribers ref) snapshot = do
  sinks <- readTVarIO ref
  remaining <-
    foldM
      ( \acc sink -> do
          r <- E.try @E.SomeException (deliver sink)
          case r of
            Right () -> pure (sink : acc)
            Left e -> do
              runStderrLoggingT $
                logWarnN
                  ( "[nodeagent] dropping VmStatusSink: "
                      <> T.pack (show e)
                  )
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

millisNow :: IO Int64
millisNow = round . (* 1000) <$> getPOSIXTime
