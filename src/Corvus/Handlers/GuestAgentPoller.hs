{-# LANGUAGE OverloadedStrings #-}

-- | Guest Agent periodic polling.
-- Spawns a background thread per VM that periodically pings the guest agent
-- and queries guest network interfaces, persisting results to the database.
module Corvus.Handlers.GuestAgentPoller
  ( startGuestAgentPoller
  , waitForFirstPing
  )
where

import Control.Concurrent (MVar, forkIO, threadDelay)
import Control.Concurrent.STM (TVar)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel, LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Qemu.GuestAgent (GuestIpAddress (..), GuestNetIf (..), guestNetworkGetInterfaces, guestPing, withGuestAgentLock)
import Corvus.Types (runFilteredLogging)
import Data.Int (Int64)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend, SqlPersistT)

-- | Start a background polling thread for a VM's guest agent.
-- Phase 1: waits 3s, then pings every 1s until first success.
-- Phase 2: queries network interfaces and pings every the configured interval.
-- Exits when the VM's PID is cleared from the database.
startGuestAgentPoller :: TVar (Map.Map Int64 (MVar ())) -> Pool SqlBackend -> QemuConfig -> Int -> Int64 -> LogLevel -> IO ()
startGuestAgentPoller gaLocks pool config intervalSec vmId logLevel = void $ forkIO $ runFilteredLogging logLevel $ do
  -- Initial delay: wait for VM to boot
  liftIO $ threadDelay 3000000
  logDebugN $ "Guest agent poller starting for VM " <> tshow vmId
  waitForAgent gaLocks pool config intervalSec vmId

-- | Block until the guest agent responds for the first time.
-- Waits 3s initial delay, then pings every 1s.
-- Transitions VmStarting → VmRunning on success.
-- Does NOT start steady-state polling (caller should start the poller separately).
waitForFirstPing :: TVar (Map.Map Int64 (MVar ())) -> Pool SqlBackend -> QemuConfig -> Int64 -> LogLevel -> IO ()
waitForFirstPing gaLocks pool config vmId logLevel = runFilteredLogging logLevel $ do
  liftIO $ threadDelay 3000000
  logDebugN $ "Waiting for first guest agent ping for VM " <> tshow vmId
  go
  where
    go = do
      alive <- isVmAlive pool vmId
      when alive $ do
        pingOk <- liftIO $ withGuestAgentLock gaLocks vmId $ guestPing config vmId
        if pingOk
          then do
            now <- liftIO getCurrentTime
            liftIO $ runSqlPool (updateHealthcheck vmId now) pool
            liftIO $ runSqlPool (transitionStartingToRunning vmId) pool
            logInfoN $ "Guest agent ready for VM " <> tshow vmId
            queryAndUpdateNetwork gaLocks pool config vmId
          else do
            liftIO $ threadDelay 1000000
            go

--------------------------------------------------------------------------------
-- Phase 1: Wait for agent readiness
--------------------------------------------------------------------------------

-- | Ping every 1s until the guest agent responds.
waitForAgent :: TVar (Map.Map Int64 (MVar ())) -> Pool SqlBackend -> QemuConfig -> Int -> Int64 -> LoggingT IO ()
waitForAgent gaLocks pool config intervalSec vmId = do
  alive <- isVmAlive pool vmId
  when alive $ do
    pingOk <- liftIO $ withGuestAgentLock gaLocks vmId $ guestPing config vmId
    if pingOk
      then do
        now <- liftIO getCurrentTime
        liftIO $ runSqlPool (updateHealthcheck vmId now) pool
        -- Transition VmStarting → VmRunning on first successful healthcheck
        liftIO $ runSqlPool (transitionStartingToRunning vmId) pool
        logInfoN $ "Guest agent ready for VM " <> tshow vmId
        -- Immediately query network interfaces on first success
        queryAndUpdateNetwork gaLocks pool config vmId
        -- Enter steady-state polling
        steadyPoll gaLocks pool config intervalSec vmId
      else do
        liftIO $ threadDelay 1000000
        waitForAgent gaLocks pool config intervalSec vmId

--------------------------------------------------------------------------------
-- Phase 2: Steady-state polling (every 10s)
--------------------------------------------------------------------------------

steadyPoll :: TVar (Map.Map Int64 (MVar ())) -> Pool SqlBackend -> QemuConfig -> Int -> Int64 -> LoggingT IO ()
steadyPoll gaLocks pool config intervalSec vmId = do
  liftIO $ threadDelay (intervalSec * 1000000)
  alive <- isVmAlive pool vmId
  when alive $ do
    pingOk <- liftIO $ withGuestAgentLock gaLocks vmId $ guestPing config vmId
    if pingOk
      then do
        now <- liftIO getCurrentTime
        liftIO $ runSqlPool (updateHealthcheck vmId now) pool
        logDebugN $ "Guest agent ping OK for VM " <> tshow vmId
        queryAndUpdateNetwork gaLocks pool config vmId
      else
        logDebugN $ "Guest agent ping failed for VM " <> tshow vmId
    steadyPoll gaLocks pool config intervalSec vmId

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
queryAndUpdateNetwork :: TVar (Map.Map Int64 (MVar ())) -> Pool SqlBackend -> QemuConfig -> Int64 -> LoggingT IO ()
queryAndUpdateNetwork gaLocks pool config vmId = do
  mGuestIfs <- liftIO $ withGuestAgentLock gaLocks vmId $ guestNetworkGetInterfaces config vmId
  case mGuestIfs of
    Nothing -> logDebugN $ "Failed to get guest interfaces for VM " <> tshow vmId
    Just guestIfs -> do
      liftIO $ runSqlPool (updateGuestNetworkData vmId guestIfs) pool
      logDebugN $
        "Updated guest network data for VM "
          <> tshow vmId
          <> " ("
          <> tshow (length guestIfs)
          <> " interfaces)"

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
        update ifKey [M.NetworkInterfaceGuestIpAddresses =. Just (formatIpAddresses (gniIpAddresses guestIf))]

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
