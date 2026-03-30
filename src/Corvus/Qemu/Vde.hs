{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VDE switch and dnsmasq process management.
-- Handles starting and stopping vde_switch processes for virtual networks.
-- When a subnet is configured, dnsmasq runs in an isolated network namespace
-- connected to the VDE switch — no root privileges required.
module Corvus.Qemu.Vde
  ( startVdeSwitch
  , stopVdeSwitch
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Control.Monad.Logger (logDebugN, logWarnN, runStdoutLoggingT)
import Corvus.Model (Network (..))
import qualified Corvus.Model as M
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Qemu.Runtime (createNetworkRuntimeDir, getDnsmasqLeaseFile, getVdeSwitchSocket)
import Corvus.Qemu.Vde.Namespace (startNamespaceManager)
import Corvus.Utils.Subnet (dhcpRangeEnd, dhcpRangeStart, gatewayAddress, prefixLength, subnetMask)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend, toSqlKey)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.IO (hClose)
import System.Posix.Signals (sigTERM, signalProcess)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, proc, waitForProcess)

-- | Start a vde_switch process for a network.
-- If a subnet is configured, starts a namespace manager that runs dnsmasq
-- in an isolated network namespace connected to the VDE switch.
-- Stores PIDs in database and forks background threads to monitor process exit.
startVdeSwitch :: QemuConfig -> Pool SqlBackend -> Int64 -> Text -> IO (Either Text ())
startVdeSwitch config pool networkId subnet = do
  let key = toSqlKey networkId :: M.NetworkId
      hasSubnet = not (T.null subnet)

  -- Create runtime directory
  _ <- createNetworkRuntimeDir config networkId
  socketPath <- getVdeSwitchSocket config networkId

  -- Spawn vde_switch (unprivileged, no TAP — TAP lives inside namespace)
  let vdeBinary = qcVdeSwitchBinary config
      vdeArgs = ["--sock", socketPath, "-mod", "777"]
      cp =
        (proc vdeBinary vdeArgs)
          { std_in = CreatePipe -- Keep stdin open so vde_switch doesn't exit on EOF
          , std_out = CreatePipe
          , std_err = CreatePipe
          , create_group = True
          }

  logCmd vdeBinary vdeArgs
  result <- try $ createProcess cp
  case result of
    Left (err :: SomeException) ->
      pure $ Left $ "Failed to start vde_switch: " <> T.pack (show err)
    Right (mStdin, _, _, ph) -> do
      mPid <- getPid ph
      case mPid of
        Nothing -> pure $ Left "Failed to get PID of vde_switch process"
        Just pid -> do
          -- Store PID in database
          runSqlPool (update key [M.NetworkVdeSwitchPid =. Just (fromIntegral pid)]) pool

          -- Fork background thread to monitor process exit.
          -- This thread also holds a reference to the stdin handle,
          -- preventing it from being GC'd and closed (which would cause
          -- vde_switch to exit with "EOF on stdin").
          _ <- forkIO $ do
            exitCode <- waitForProcess ph
            -- Close stdin handle now that the process has exited
            forM_ mStdin hClose
            runStdoutLoggingT $
              logWarnN $
                "vde_switch for network " <> T.pack (show networkId) <> " exited with " <> T.pack (show exitCode)
            -- Stop namespace manager if still running and clear all PIDs
            stopNetwork config pool networkId

          -- Wait for socket to become available
          waitForSocket socketPath

          -- Start namespace manager with dnsmasq if subnet is configured
          if hasSubnet
            then do
              nsResult <- startNamespaceManagerForNetwork config pool networkId subnet socketPath
              case nsResult of
                Left err -> do
                  -- Roll back: stop vde_switch
                  void $ killProcess (fromIntegral pid)
                  runSqlPool (update key [M.NetworkVdeSwitchPid =. Nothing]) pool
                  pure $ Left err
                Right () -> pure $ Right ()
            else pure $ Right ()

-- | Stop a vde_switch process (and namespace manager if running) for a network.
-- Sends SIGTERM and clears PIDs from database.
stopVdeSwitch :: QemuConfig -> Pool SqlBackend -> Int64 -> IO (Either Text ())
stopVdeSwitch config pool networkId = do
  let key = toSqlKey networkId :: M.NetworkId
  mNetwork <- runSqlPool (get key) pool
  case mNetwork of
    Nothing -> pure $ Left "Network not found"
    Just network -> case networkVdeSwitchPid network of
      Nothing -> pure $ Left "Network is not running"
      Just vdePid -> do
        -- Stop namespace manager first if running
        forM_ (networkDnsmasqPid network) $ \nsPid -> do
          killProcess nsPid
          runSqlPool (update key [M.NetworkDnsmasqPid =. Nothing]) pool

        -- Stop vde_switch
        result <- killProcess vdePid
        case result of
          Left err -> do
            -- Process might already be dead — clear PID anyway
            runSqlPool (update key [M.NetworkVdeSwitchPid =. Nothing]) pool
            pure $ Left $ "Error stopping vde_switch: " <> err
          Right () -> do
            -- Clear PID
            runSqlPool (update key [M.NetworkVdeSwitchPid =. Nothing]) pool
            -- Clean up runtime directory
            cleanupNetworkRuntime config networkId
            pure $ Right ()

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Start a namespace manager for a network with a configured subnet.
-- The namespace manager creates a network namespace, connects to the VDE switch,
-- and runs dnsmasq inside the namespace.
startNamespaceManagerForNetwork :: QemuConfig -> Pool SqlBackend -> Int64 -> Text -> FilePath -> IO (Either Text ())
startNamespaceManagerForNetwork config pool networkId subnet socketPath = do
  let key = toSqlKey networkId :: M.NetworkId
  leaseFile <- getDnsmasqLeaseFile config networkId

  case (,,,,) <$> gatewayAddress subnet <*> prefixLength subnet <*> dhcpRangeStart subnet <*> dhcpRangeEnd subnet <*> subnetMask subnet of
    Left err -> pure $ Left $ "Subnet error: " <> err
    Right (gw, prefix, rangeStart, rangeEnd, mask) -> do
      let dnsmasqBin = qcDnsmasqBinary config
      runStdoutLoggingT $
        logDebugN $
          "[namespace] starting for network " <> T.pack (show networkId) <> " on " <> gw <> "/" <> prefix
      nsResult <- startNamespaceManager socketPath dnsmasqBin leaseFile gw prefix rangeStart rangeEnd mask
      case nsResult of
        Left err ->
          pure $ Left $ "Failed to start namespace manager: " <> err
        Right nsPid -> do
          -- Store namespace manager PID (reuses dnsmasqPid field)
          runSqlPool (update key [M.NetworkDnsmasqPid =. Just (fromIntegral nsPid)]) pool

          pure $ Right ()

-- | Wait for the vde_switch control directory to appear (up to 5 seconds).
-- vde_switch creates a directory at the socket path containing ctl and data sockets.
waitForSocket :: FilePath -> IO ()
waitForSocket path = go 50
  where
    go :: Int -> IO ()
    go 0 = pure () -- Give up silently
    go n = do
      exists <- doesDirectoryExist path
      if exists
        then pure ()
        else do
          threadDelay 100000 -- 100ms
          go (n - 1)

-- | Stop all processes for a network and clear PIDs in the database.
-- Used by monitor threads when a process exits unexpectedly.
stopNetwork :: QemuConfig -> Pool SqlBackend -> Int64 -> IO ()
stopNetwork config pool networkId = do
  let key = toSqlKey networkId :: M.NetworkId
  mNetwork <- runSqlPool (get key) pool
  forM_ mNetwork $ \network -> do
    -- Kill namespace manager if running
    forM_ (networkDnsmasqPid network) $ \nsPid ->
      void $ killProcess nsPid
    -- Kill vde_switch if running
    forM_ (networkVdeSwitchPid network) $ \vdePid ->
      void $ killProcess vdePid
    -- Clear all PIDs
    runSqlPool
      ( update
          key
          [ M.NetworkVdeSwitchPid =. Nothing
          , M.NetworkDnsmasqPid =. Nothing
          ]
      )
      pool
    -- Clean up runtime directory
    cleanupNetworkRuntime config networkId

-- | Kill a process by sending SIGTERM. No privilege escalation needed
-- since all processes are owned by the current user.
killProcess :: Int -> IO (Either Text ())
killProcess pid = do
  runStdoutLoggingT $
    logDebugN $
      "[kill] sending SIGTERM to " <> T.pack (show pid)
  result <- try $ signalProcess sigTERM (fromIntegral pid)
  case result of
    Left (err :: SomeException) ->
      pure $ Left $ "Failed to kill process " <> T.pack (show pid) <> ": " <> T.pack (show err)
    Right () ->
      pure $ Right ()

-- | Log a command before executing it
logCmd :: String -> [String] -> IO ()
logCmd binary args = runStdoutLoggingT $ do
  let cmd = T.pack $ unwords (binary : args)
  logDebugN $ "[cmd] " <> cmd

-- | Clean up network runtime directory
cleanupNetworkRuntime :: QemuConfig -> Int64 -> IO ()
cleanupNetworkRuntime config networkId = do
  socketPath <- getVdeSwitchSocket config networkId
  -- vde_switch creates a directory at the socket path
  result <- try $ removeDirectoryRecursive socketPath
  case result of
    Left (_ :: SomeException) -> pure ()
    Right () -> pure ()
