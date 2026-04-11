{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Daemon lifecycle handlers: startup and graceful shutdown.
-- Both are Actions that create subtasks for individual operations
-- (network start, VM start, process cleanup, etc.).
module Corvus.Handlers.Lifecycle
  ( -- * Action types
    Startup (..)
  , GracefulShutdown (..)
  , StartNamespaceAction (..)
  , StartPastaAction (..)
  , SetupNatInfra (..)
  )
where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Network (NetworkStart (..))
import Corvus.Handlers.Vm (VmStart (..))
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Netns (startNamespace)
import Corvus.Qemu.Netns.Manager (destroyBridge, enableIpForwarding, setupNatTable, startPasta, stopDnsmasq, stopPasta, teardownNatTable)
import Corvus.Qemu.Process (killVmProcess)
import Corvus.Qemu.Virtiofsd (killVirtiofsdProcesses)
import Corvus.Types
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
      nsResp <- liftIO $ runActionAsSubtask state StartNamespaceAction taskKey
      case classifyResponse nsResp of
        (TaskError, Just err) ->
          logWarnN $ "Failed to start network namespace: " <> err
        _ -> do
          mNsPidStarted <- liftIO $ readTVarIO (ssNamespacePid state)
          case mNsPidStarted of
            Nothing -> logWarnN "Namespace started but PID not found in TVar"
            Just nsPidInt -> do
              logInfoN $ "Network namespace started (PID " <> T.pack (show nsPidInt) <> ")"

              -- Start pasta for NAT support (subtask)
              pastaResp <- liftIO $ runActionAsSubtask state (StartPastaAction nsPidInt) taskKey
              case classifyResponse pastaResp of
                (TaskError, Just err) ->
                  logWarnN $ "Failed to start pasta (NAT unavailable): " <> err
                _ -> do
                  mPastaPidStarted <- liftIO $ readTVarIO (ssPastaPid state)
                  case mPastaPidStarted of
                    Nothing -> logWarnN "Pasta started but PID not found in TVar"
                    Just pastaPid -> do
                      logInfoN $ "pasta started for NAT (PID " <> T.pack (show pastaPid) <> ")"
                      -- Enable IP forwarding and setup NAT table (subtask)
                      _ <- liftIO $ runActionAsSubtask state (SetupNatInfra nsPidInt) taskKey
                      logInfoN "IP forwarding enabled, nftables NAT table created"

      -- Autostart networks (before VMs, since VMs may depend on networks)
      autostartNetworks <- liftIO $ runSqlPool (selectList [M.NetworkAutostart ==. True] [Asc M.NetworkName]) pool
      unless (null autostartNetworks) $ do
        logInfoN $ "Autostarting " <> T.pack (show (length autostartNetworks)) <> " network(s)"
        liftIO $ forM_ autostartNetworks $ \(Entity nwKey nw) -> do
          let nwId = fromSqlKey nwKey
          nwResp <- runActionAsSubtask state (NetworkStart nwId) taskKey
          runServerLogging state $ case classifyResponse nwResp of
            (TaskError, Just err) -> logWarnN $ "Failed to autostart network " <> networkName nw <> ": " <> err
            _ -> logInfoN $ "Autostarted network " <> networkName nw

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
            vmResp <- runActionAsSubtask state (VmStart vmId) taskKey
            runServerLogging state $ case classifyResponse vmResp of
              (TaskError, Just err) -> logWarnN $ "Failed to autostart VM " <> vmName vm <> ": " <> err
              _ -> logInfoN $ "Autostarted VM " <> vmName vm

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

--------------------------------------------------------------------------------
-- Infrastructure Action Types (startup subtasks)
--------------------------------------------------------------------------------

-- | Start the global network namespace.
-- Writes the namespace PID to the ServerState TVar on success.
data StartNamespaceAction = StartNamespaceAction

instance Action StartNamespaceAction where
  actionSubsystem _ = SubSystem
  actionCommand _ = "start-namespace"
  actionExecute ctx StartNamespaceAction = do
    let state = acState ctx
    result <- fmap (fmap fromIntegral) startNamespace
    case result of
      Left err -> pure $ RespError err
      Right nsPid -> do
        atomically $ writeTVar (ssNamespacePid state) (Just nsPid)
        pure RespOk

-- | Start pasta for NAT support inside the namespace.
-- Writes the pasta PID to the ServerState TVar on success.
newtype StartPastaAction = StartPastaAction {spaNsPid :: Int}

instance Action StartPastaAction where
  actionSubsystem _ = SubSystem
  actionCommand _ = "start-pasta"
  actionExecute ctx (StartPastaAction nsPid) = do
    let state = acState ctx
    result <- startPasta nsPid (ssQemuConfig state)
    case result of
      Left err -> pure $ RespError err
      Right pastaPid -> do
        atomically $ writeTVar (ssPastaPid state) (Just pastaPid)
        pure RespOk

-- | Enable IP forwarding and setup nftables NAT table inside the namespace.
newtype SetupNatInfra = SetupNatInfra {sniNsPid :: Int}

instance Action SetupNatInfra where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "setup-nat"
  actionExecute ctx (SetupNatInfra nsPid) = do
    let state = acState ctx
    fwdResult <- enableIpForwarding nsPid
    case fwdResult of
      Left err -> pure $ RespError $ "IP forwarding: " <> err
      Right () -> do
        natResult <- setupNatTable nsPid (ssQemuConfig state)
        case natResult of
          Left err -> pure $ RespError err
          Right () -> pure RespOk
