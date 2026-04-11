{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Daemon lifecycle handlers: startup and graceful shutdown.
-- Both are Actions that create subtasks for individual operations
-- (network start, VM start, process cleanup, etc.).
module Corvus.Handlers.Lifecycle
  ( -- * Action types
    Startup (..)
  , GracefulShutdown (..)
  )
where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Network (handleNetworkStart)
import Corvus.Handlers.Subtask (SubtaskSpec (..), withOptionalSubtask)
import Corvus.Handlers.Vm (handleVmStartExecute, handleVmStartValidate)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Netns (startNamespace)
import Corvus.Qemu.Netns.Manager (destroyBridge, enableIpForwarding, setupNatTable, startPasta, stopDnsmasq, stopPasta, teardownNatTable)
import Corvus.Qemu.Process (killVmProcess)
import Corvus.Qemu.Virtiofsd (killVirtiofsdProcesses)
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (fromSqlKey)
import System.Posix.Process (ProcessStatus, getProcessStatus)
import System.Posix.Signals (sigTERM, signalProcess)

--------------------------------------------------------------------------------
-- Startup Action
--------------------------------------------------------------------------------

-- | Daemon startup: clean stale state, start namespace, autostart networks and VMs.
newtype Startup = Startup {startupRetentionDays :: Int}

instance Action Startup where
  actionSubsystem _ = SubSystem
  actionCommand _ = "startup"
  actionExecute ctx (Startup retentionDays) = do
    let state = acState ctx
        taskKey = acTaskId ctx
        pool = ssDbPool state
    now <- getCurrentTime

    runServerLogging state $ do
      -- Mark stale "running" and "not_started" tasks as error (daemon crashed while processing)
      liftIO $ runSqlPool (updateWhere [TaskResult <-. [TaskRunning, TaskNotStarted], TaskId !=. taskKey] [TaskResult =. TaskError, TaskMessage =. Just "Daemon restarted"]) pool
      logInfoN "Marked stale running tasks as error"

      -- Kill orphaned QEMU processes and reset stale VMs
      staleVms <- liftIO $ runSqlPool (selectList [M.VmStatus <-. [VmStarting, VmRunning, VmStopping, VmPaused]] []) pool
      liftIO $ forM_ staleVms $ \(Entity vmKey vm) -> do
        case vmPid vm of
          Just pid -> runServerLogging state $ do
            logInfoN $ "Killing orphaned QEMU process for VM " <> vmName vm <> " (PID " <> T.pack (show pid) <> ")"
            _ <- killVmProcess (fromSqlKey vmKey) pid
            killVirtiofsdProcesses pool (fromSqlKey vmKey)
          Nothing -> pure ()
      liftIO $ runSqlPool (updateWhere [M.VmStatus <-. [VmStarting, VmRunning, VmStopping, VmPaused]] [M.VmStatus =. VmError, M.VmPid =. Nothing, M.VmHealthcheck =. Nothing]) pool
      logInfoN "Reset stale VMs to error state"

      -- Kill orphaned dnsmasq processes, reset network state
      staleNetworks <- liftIO $ runSqlPool (selectList ([M.NetworkRunning ==. True] ||. [M.NetworkDnsmasqPid !=. Nothing]) []) pool
      liftIO $ forM_ staleNetworks $ \(Entity _nwKey nw) ->
        forM_ (networkDnsmasqPid nw) stopDnsmasq
      liftIO $ runSqlPool (updateWhere [M.NetworkRunning ==. True] [M.NetworkRunning =. False, M.NetworkDnsmasqPid =. Nothing]) pool
      logInfoN "Reset stale network state"

      -- Start the global network namespace (subtask)
      logInfoN "Starting network namespace"
      nsResult <- liftIO $ do
        let spec = SubtaskSpec SubSystem "start-namespace" Nothing
        withOptionalSubtask
          pool
          (Just taskKey)
          spec
          (fmap (fmap fromIntegral) startNamespace)
          (Just . fromIntegral)
      case nsResult of
        Left err ->
          logWarnN $ "Failed to start network namespace: " <> err
        Right nsPidInt -> do
          liftIO $ atomically $ writeTVar (ssNamespacePid state) (Just nsPidInt)
          logInfoN $ "Network namespace started (PID " <> T.pack (show nsPidInt) <> ")"

          -- Start pasta for NAT support (subtask)
          pastaResult <- liftIO $ do
            let spec = SubtaskSpec SubSystem "start-pasta" Nothing
            withOptionalSubtask
              pool
              (Just taskKey)
              spec
              (startPasta nsPidInt (ssQemuConfig state))
              (Just . fromIntegral)
          case pastaResult of
            Left err ->
              logWarnN $ "Failed to start pasta (NAT unavailable): " <> err
            Right pastaPid -> do
              liftIO $ atomically $ writeTVar (ssPastaPid state) (Just pastaPid)
              logInfoN $ "pasta started for NAT (PID " <> T.pack (show pastaPid) <> ")"
              -- Enable IP forwarding and setup NAT table (subtask)
              _ <- liftIO $ do
                let spec = SubtaskSpec SubNetwork "setup-nat" Nothing
                withOptionalSubtask
                  pool
                  (Just taskKey)
                  spec
                  ( do
                      fwdResult <- enableIpForwarding nsPidInt
                      case fwdResult of
                        Left err -> pure $ Left $ "IP forwarding: " <> err
                        Right () -> setupNatTable nsPidInt (ssQemuConfig state)
                  )
                  (const Nothing)
              logInfoN "IP forwarding enabled, nftables NAT table created"

      -- Autostart networks (before VMs, since VMs may depend on networks)
      autostartNetworks <- liftIO $ runSqlPool (selectList [M.NetworkAutostart ==. True] [Asc M.NetworkName]) pool
      unless (null autostartNetworks) $ do
        logInfoN $ "Autostarting " <> T.pack (show (length autostartNetworks)) <> " network(s)"
        liftIO $ forM_ autostartNetworks $ \(Entity nwKey nw) -> do
          let nwId = fromSqlKey nwKey
              spec = SubtaskSpec SubNetwork "autostart" (Just $ networkName nw)
          nwResult <-
            withOptionalSubtask
              pool
              (Just taskKey)
              spec
              ( do
                  resp <- handleNetworkStart state nwId taskKey
                  pure $ case resp of
                    RespNetworkStarted -> Right ()
                    RespNetworkAlreadyRunning -> Right ()
                    RespNetworkError err -> Left err
                    _ -> Left $ "Unexpected response: " <> T.pack (show resp)
              )
              (const Nothing)
          runServerLogging state $ case nwResult of
            Right () -> logInfoN $ "Autostarted network " <> networkName nw
            Left err -> logWarnN $ "Failed to autostart network " <> networkName nw <> ": " <> err

      -- Autostart VMs (after networks are up)
      autostartVms <- liftIO $ runSqlPool (selectList [M.VmAutostart ==. True] [Asc M.VmName]) pool
      unless (null autostartVms) $ do
        logInfoN $ "Autostarting " <> T.pack (show (length autostartVms)) <> " VM(s)"
        liftIO $ forM_ autostartVms $ \(Entity vmKey vm) -> do
          let vmId = fromSqlKey vmKey
          -- Only autostart VMs that are stopped
          when (vmStatus vm == VmStopped) $ do
            -- Reset error state VMs to stopped first
            when (vmStatus vm == VmError) $
              runSqlPool (update vmKey [M.VmStatus =. VmStopped]) pool
            let spec = SubtaskSpec SubVm "autostart" (Just $ vmName vm)
            vmResult <-
              withOptionalSubtask
                pool
                (Just taskKey)
                spec
                ( do
                    validated <- handleVmStartValidate state vmId
                    case validated of
                      Left errResp -> pure $ Left $ T.pack $ show errResp
                      Right _ -> do
                        resp <- handleVmStartExecute state vmId taskKey
                        pure $ case resp of
                          RespVmStateChanged _ -> Right ()
                          _ -> Left $ T.pack $ show resp
                )
                (const Nothing)
            runServerLogging state $ case vmResult of
              Right () -> logInfoN $ "Autostarted VM " <> vmName vm
              Left err -> logWarnN $ "Failed to autostart VM " <> vmName vm <> ": " <> err

      -- Delete old task entries
      when (retentionDays > 0) $ do
        let cutoff = addUTCTime (fromIntegral $ negate $ retentionDays * 86400) now
        liftIO $ runSqlPool (deleteWhere [TaskStartedAt <. cutoff]) pool
        logInfoN $ "Cleaned up tasks older than " <> T.pack (show retentionDays) <> " days"

    pure RespStartupComplete

--------------------------------------------------------------------------------
-- Graceful Shutdown Action
--------------------------------------------------------------------------------

-- | Daemon shutdown: stop all running VMs and networks, teardown namespace.
data GracefulShutdown = GracefulShutdown

instance Action GracefulShutdown where
  actionSubsystem _ = SubSystem
  actionCommand _ = "shutdown"
  actionExecute ctx GracefulShutdown = do
    let state = acState ctx
        taskKey = acTaskId ctx
        pool = ssDbPool state

    runServerLogging state $ do
      -- Stop all running/starting VMs
      runningVms <- liftIO $ runSqlPool (selectList [M.VmStatus <-. [VmStarting, VmRunning, VmPaused]] []) pool
      logInfoN $ "Stopping " <> T.pack (show (length runningVms)) <> " running VM(s)"
      liftIO $ forM_ runningVms $ \(Entity vmKey vm) -> do
        case vmPid vm of
          Just pid -> runServerLogging state $ do
            logInfoN $ "Killing VM " <> vmName vm <> " (PID " <> T.pack (show pid) <> ")"
            -- Clear PID first (so process monitor thread doesn't interfere)
            liftIO $ runSqlPool (update vmKey [M.VmPid =. Nothing]) pool
            _ <- killVmProcess (fromSqlKey vmKey) pid
            killVirtiofsdProcesses pool (fromSqlKey vmKey)
            liftIO $ runSqlPool (update vmKey [M.VmStatus =. VmStopped, M.VmHealthcheck =. Nothing]) pool
          Nothing -> pure ()
      -- Also handle VMs in Stopping state (QEMU still running)
      stoppingVms <- liftIO $ runSqlPool (selectList [M.VmStatus ==. VmStopping] []) pool
      liftIO $ forM_ stoppingVms $ \(Entity vmKey vm) -> do
        case vmPid vm of
          Just pid -> runServerLogging state $ do
            logInfoN $ "Force-killing stopping VM " <> vmName vm <> " (PID " <> T.pack (show pid) <> ")"
            liftIO $ runSqlPool (update vmKey [M.VmPid =. Nothing]) pool
            _ <- killVmProcess (fromSqlKey vmKey) pid
            killVirtiofsdProcesses pool (fromSqlKey vmKey)
            liftIO $ runSqlPool (update vmKey [M.VmStatus =. VmStopped, M.VmHealthcheck =. Nothing]) pool
          Nothing -> pure ()

      -- Stop all running networks (dnsmasq + bridges)
      runningNetworks <- liftIO $ runSqlPool (selectList [M.NetworkRunning ==. True] []) pool
      logInfoN $ "Stopping " <> T.pack (show (length runningNetworks)) <> " running network(s)"
      mNsPid <- liftIO $ readTVarIO (ssNamespacePid state)
      liftIO $ forM_ runningNetworks $ \(Entity nwKey nw) -> do
        forM_ (networkDnsmasqPid nw) stopDnsmasq
        case mNsPid of
          Just nsPid -> void $ destroyBridge nsPid (fromSqlKey nwKey)
          Nothing -> pure ()
        runSqlPool (update nwKey [M.NetworkRunning =. False, M.NetworkDnsmasqPid =. Nothing]) pool

      -- Teardown nftables NAT table
      case mNsPid of
        Just nsPid -> do
          _ <- liftIO $ teardownNatTable nsPid (ssQemuConfig state)
          pure ()
        Nothing -> pure ()

      -- Kill pasta if running
      mPastaPid <- liftIO $ readTVarIO (ssPastaPid state)
      case mPastaPid of
        Just pastaPid -> do
          logInfoN $ "Killing pasta (PID " <> T.pack (show pastaPid) <> ")"
          liftIO $ stopPasta pastaPid
          liftIO $ atomically $ writeTVar (ssPastaPid state) Nothing
        Nothing -> pure ()

      -- Kill the network namespace manager
      case mNsPid of
        Just nsPid -> do
          logInfoN $ "Killing namespace manager (PID " <> T.pack (show nsPid) <> ")"
          result' <- liftIO $ try $ signalProcess sigTERM (fromIntegral nsPid)
          case result' of
            Left (_ :: SomeException) -> pure ()
            Right () -> do
              -- Reap the child process to avoid zombie
              _ <- liftIO (try (getProcessStatus True False (fromIntegral nsPid)) :: IO (Either SomeException (Maybe ProcessStatus)))
              pure ()
          liftIO $ atomically $ writeTVar (ssNamespacePid state) Nothing
        Nothing -> pure ()

      -- Mark any remaining running tasks as error
      liftIO $ runSqlPool (updateWhere [TaskResult <-. [TaskRunning, TaskNotStarted], TaskId !=. taskKey] [TaskResult =. TaskError, TaskMessage =. Just "Daemon shutting down"]) pool
      logInfoN "Graceful shutdown complete"

    pure RespShutdownComplete
