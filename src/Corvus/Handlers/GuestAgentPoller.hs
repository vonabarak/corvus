{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Guest Agent periodic polling.
-- Spawns a background thread per VM that periodically pings the guest agent
-- and queries guest network interfaces, persisting results to the database.
module Corvus.Handlers.GuestAgentPoller
  ( startGuestAgentPoller
  , waitForFirstPing
  , pushGuestAgentStatus
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as CGS
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (SomeException)
import Control.Monad (forM_, void, when)
import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel, LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Node.GuestAgent (GuestAgentConns, GuestIpAddress (..), GuestNetIf (..), guestNetworkGetInterfaces, guestPing)
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Rpc.Streams (callSink)
import Corvus.Types (ServerState (..), runFilteredLogging)
import Data.Int (Int64)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend, SqlPersistT)

-- | Start a background polling thread for a VM's guest agent.
-- Called after 'waitForFirstPing' has confirmed the agent is responsive.
-- Immediately queries guest network interfaces, then enters steady-state
-- polling every @intervalSec@ seconds. Exits when the VM's PID is cleared
-- from the database.
startGuestAgentPoller :: ServerState -> Int -> Int64 -> IO ()
startGuestAgentPoller state intervalSec vmId =
  void $ forkIO $ runFilteredLogging (ssLogLevel state) $ do
    logDebugN $ "Guest agent poller starting for VM " <> tshow vmId
    -- queryAndUpdateNetwork can raise (DB timeout etc.); don't let an early
    -- exception silently kill the forked thread and skip steady-state polling.
    safeQueryAndUpdateNetwork (ssGuestAgentConns state) (ssDbPool state) (ssQemuConfig state) vmId
    steadyPoll state intervalSec vmId

-- | Call 'queryAndUpdateNetwork' and swallow any exception with a warning.
-- Without this, one failed DB call on the first iteration would stop the
-- whole poller thread, leaving guest IPs permanently stale.
safeQueryAndUpdateNetwork
  :: GuestAgentConns -> Pool SqlBackend -> QemuConfig -> Int64 -> LoggingT IO ()
safeQueryAndUpdateNetwork conns pool config vmId = do
  r <- MC.try (queryAndUpdateNetwork conns pool config vmId)
  case r of
    Right () -> pure ()
    Left (e :: SomeException) ->
      logWarnN $ "queryAndUpdateNetwork failed for VM " <> tshow vmId <> ": " <> T.pack (show e)

-- | Block until the guest agent responds for the first time.
-- Waits 3s initial delay, then pings every 1s. On success, updates the
-- healthcheck timestamp and transitions @VmStarting@ → @VmRunning@. Returns
-- immediately once the transition is made — the network interface query is
-- deferred to 'startGuestAgentPoller' so @crv vm start --wait@ is not held
-- up by a slow @guest-network-get-interfaces@ call.
waitForFirstPing :: ServerState -> Int64 -> IO ()
waitForFirstPing state vmId = runFilteredLogging (ssLogLevel state) $ do
  liftIO $ threadDelay 3000000
  logDebugN $ "Waiting for first guest agent ping for VM " <> tshow vmId
  go
  where
    pool = ssDbPool state
    conns = ssGuestAgentConns state
    config = ssQemuConfig state
    go = do
      alive <- isVmAlive pool vmId
      when alive $ do
        pingOk <- liftIO $ guestPing conns config vmId
        liftIO $ pushGuestAgentStatus state vmId pingOk
        if pingOk
          then do
            now <- liftIO getCurrentTime
            liftIO $ runSqlPool (updateHealthcheck vmId now) pool
            liftIO $ runSqlPool (transitionStartingToRunning vmId) pool
            logInfoN $ "Guest agent ready for VM " <> tshow vmId
          else do
            liftIO $ threadDelay 1000000
            go

--------------------------------------------------------------------------------
-- Steady-state polling
--------------------------------------------------------------------------------

steadyPoll :: ServerState -> Int -> Int64 -> LoggingT IO ()
steadyPoll state intervalSec vmId = do
  liftIO $ threadDelay (intervalSec * 1000000)
  alive <- isVmAlive pool vmId
  when alive $ do
    pingOk <- liftIO $ guestPing conns config vmId
    liftIO $ pushGuestAgentStatus state vmId pingOk
    if pingOk
      then do
        now <- liftIO getCurrentTime
        liftIO $ runSqlPool (updateHealthcheck vmId now) pool
        logDebugN $ "Guest agent ping OK for VM " <> tshow vmId
        safeQueryAndUpdateNetwork conns pool config vmId
      else
        logDebugN $ "Guest agent ping failed for VM " <> tshow vmId
    steadyPoll state intervalSec vmId
  where
    pool = ssDbPool state
    conns = ssGuestAgentConns state
    config = ssQemuConfig state

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Check if the VM still has a PID (i.e., is still running).
isVmAlive :: Pool SqlBackend -> Int64 -> LoggingT IO Bool
isVmAlive pool vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- liftIO $ runSqlPool (get key) pool
  case mVm >>= vmPid of
    Nothing -> do
      logDebugN $ "Guest agent poller exiting for VM " <> tshow vmId <> " (no PID)"
      pure False
    Just _ -> pure True

-- | Query guest network interfaces and update matching DB rows by MAC address.
queryAndUpdateNetwork :: GuestAgentConns -> Pool SqlBackend -> QemuConfig -> Int64 -> LoggingT IO ()
queryAndUpdateNetwork conns pool config vmId = do
  mGuestIfs <- liftIO $ guestNetworkGetInterfaces conns config vmId
  case mGuestIfs of
    Nothing -> logDebugN $ "Failed to get guest interfaces for VM " <> tshow vmId
    Just guestIfs -> do
      logDebugN $
        "VM "
          <> tshow vmId
          <> " guest agent reports "
          <> tshow (length guestIfs)
          <> " interfaces: "
          <> T.intercalate ", " (map describeIface guestIfs)
      liftIO $ runSqlPool (updateGuestNetworkData vmId guestIfs) pool
  where
    describeIface g =
      gniHardwareAddress g
        <> "="
        <> T.intercalate "|" (map (\ip -> giaAddress ip <> "/" <> tshow (giaPrefix ip)) (gniIpAddresses g))

-- | Update the healthcheck timestamp on the VM.
updateHealthcheck :: Int64 -> UTCTime -> SqlPersistT IO ()
updateHealthcheck vmId now = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmHealthcheck =. Just now]

-- | Update guest IP addresses on network interfaces by matching MAC addresses.
-- Clears guest data on interfaces that don't match any guest-reported interface.
updateGuestNetworkData :: Int64 -> [GuestNetIf] -> SqlPersistT IO ()
updateGuestNetworkData vmId guestIfs = do
  let vmKey = toSqlKey vmId :: VmId
  hostIfs <- selectList [M.NetworkInterfaceVmId ==. vmKey] []
  forM_ hostIfs $ \(Entity ifKey hostIf) -> do
    let hostMac = T.toLower (networkInterfaceMacAddress hostIf)
        mGuestIf = find (\g -> T.toLower (gniHardwareAddress g) == hostMac) guestIfs
    case mGuestIf of
      Nothing ->
        update ifKey [M.NetworkInterfaceGuestIpAddresses =. Nothing]
      Just guestIf ->
        let ips = gniIpAddresses guestIf
         in update ifKey [M.NetworkInterfaceGuestIpAddresses =. if null ips then Nothing else Just (formatIpAddresses ips)]

-- | Format IP addresses as comma-separated "addr/prefix" strings.
-- e.g. "10.0.0.5/24,fd00::5/64"
formatIpAddresses :: [GuestIpAddress] -> Text
formatIpAddresses = T.intercalate "," . map (\ip -> giaAddress ip <> "/" <> tshow (giaPrefix ip))

-- | Transition VM from VmStarting to VmRunning (only if currently VmStarting).
transitionStartingToRunning :: Int64 -> SqlPersistT IO ()
transitionStartingToRunning vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  case mVm of
    Just vm
      | vmStatus vm == VmStarting ->
          update key [M.VmStatus =. VmRunning]
    _ -> pure ()

-- | Show helper for Text conversion.
tshow :: (Show a) => a -> Text
tshow = T.pack . show

--------------------------------------------------------------------------------
-- Subscriber fan-out
--------------------------------------------------------------------------------

-- | Push a 'GuestAgentStatus' to every subscriber currently
-- registered for this VM via @vm.subscribeGuestAgent@. Subscribers
-- whose @push@ call raises an exception (cap dropped on the
-- client side) are removed from the list — this keeps the
-- subscriber set self-pruning without a separate Handle-drop hook.
pushGuestAgentStatus :: ServerState -> Int64 -> Bool -> IO ()
pushGuestAgentStatus state vmId reachable = do
  subs <- readTVarIO (ssGuestAgentSubs state)
  let sinks = Map.findWithDefault [] vmId subs
  case sinks of
    [] -> pure ()
    _ -> do
      now <- getCurrentTime
      mVm <- runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
      let nanos t = floor (nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds t) * 1e9) :: Int64
          lastHc =
            maybe 0 nanos $
              if reachable then Just now else vmHealthcheck =<< mVm
          status :: C.Parsed CGS.GuestAgentStatus
          status =
            CGS.GuestAgentStatus
              { CGS.vmId = vmId
              , CGS.lastHealthcheck = lastHc
              , CGS.enabled = maybe False vmGuestAgent mVm
              , CGS.reachable = reachable
              , CGS.message = T.empty
              }
          params = CGS.GuestAgentStatusSink'push'params {CGS.status = status}
      alive <- traverse (tryPush params) sinks
      atomically $
        modifyTVar' (ssGuestAgentSubs state) $
          Map.insert vmId (map fst (filter snd (zip sinks alive)))
  where
    tryPush params sink = do
      r <- MC.try (callSink #push params sink) :: IO (Either SomeException ())
      pure $ case r of
        Right () -> True
        Left _ -> False
