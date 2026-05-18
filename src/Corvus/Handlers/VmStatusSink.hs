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
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Streams as CGS
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import qualified Control.Exception as E
import Control.Monad (forM_, when)
import qualified Corvus.Model as M
import Corvus.Rpc.Common (handleParsed)
import Corvus.Rpc.Streams (callSink)
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT)

-- | Server-side handle. Owns a reference to 'ServerState' so the
-- 'onSnapshot' handler can reach the DB pool, the
-- @vm.subscribeGuestAgent@ subscriber map, and (for IP-formatting)
-- nothing else — the snapshot is self-contained otherwise.
newtype DaemonVmStatusSink = DaemonVmStatusSink
  { dvssState :: ServerState
  }

newDaemonVmStatusSink :: ServerState -> DaemonVmStatusSink
newDaemonVmStatusSink = DaemonVmStatusSink

instance SomeServer DaemonVmStatusSink

instance CGNA.VmStatusSink'server_ DaemonVmStatusSink where
  vmStatusSink'onSnapshot (DaemonVmStatusSink state) =
    handleParsed $ \CGNA.VmStatusSink'onSnapshot'params {CGNA.snapshot = snap} -> do
      let CGNA.VmStatusSnapshot {CGNA.entries = entries} = snap
      forM_ entries (processEntry state)
      pure CGNA.VmStatusSink'onSnapshot'results

-- | Reconcile one 'VmStatusEntry' with daemon state.
processEntry :: ServerState -> C.Parsed CGNA.VmStatusEntry -> IO ()
processEntry state entry = do
  let CGNA.VmStatusEntry
        { CGNA.vmId = vmId
        , CGNA.guestAgentOk = ok
        , CGNA.lastPingMillis = pingMillis
        , CGNA.netIfs = ifs
        } = entry
      pool = ssDbPool state
  -- Stamp healthcheck on successful ping; reflect MAC→IP map in
  -- the network_interface table.
  when ok $ do
    let pingedAt = millisToUtc pingMillis
    runSqlPool (updateHealthcheck vmId pingedAt) pool
    runSqlPool (updateGuestNetworkData vmId ifs) pool
  -- Fan out the per-VM GuestAgentStatus to anyone subscribed via
  -- @vm.subscribeGuestAgent@. Mirrors the old in-daemon poller's
  -- 'pushGuestAgentStatus' helper.
  pushGuestAgentStatus state vmId ok pingMillis

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
        E.try (callSink #push params sink)
          :: IO (Either E.SomeException ())
      pure $ case r of
        Right () -> True
        Left _ -> False

millisToUtc :: Int64 -> UTCTime
millisToUtc ms =
  posixSecondsToUTCTime
    (realToFrac (fromIntegral ms / 1000 :: Double))

tshow :: (Show a) => a -> Text
tshow = T.pack . show
