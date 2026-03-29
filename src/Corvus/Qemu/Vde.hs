{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VDE switch and dnsmasq process management.
-- Handles starting and stopping vde_switch processes for virtual networks,
-- with optional TAP interface configuration and DHCP via dnsmasq.
module Corvus.Qemu.Vde
  ( startVdeSwitch
  , stopVdeSwitch
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import Control.Monad.Logger (logDebugN, logWarnN, runStdoutLoggingT)
import Corvus.Model (Network (..))
import qualified Corvus.Model as M
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Qemu.Runtime (createNetworkRuntimeDir, getDnsmasqLeaseFile, getTapInterfaceName, getVdeSwitchSocket)
import Corvus.Utils.Subnet (dhcpRangeEnd, dhcpRangeStart, gatewayAddress, prefixLength, subnetMask)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend, toSqlKey)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.IO (hClose, hGetContents)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, proc, waitForProcess)

-- | Start a vde_switch process for a network.
-- If a subnet is configured, also creates a TAP interface and starts dnsmasq.
-- Stores PIDs in database and forks background threads to monitor process exit.
startVdeSwitch :: QemuConfig -> Pool SqlBackend -> Int64 -> Text -> IO (Either Text ())
startVdeSwitch config pool networkId subnet = do
  let key = toSqlKey networkId :: M.NetworkId
      hasSubnet = not (T.null subnet)
      tapName = getTapInterfaceName networkId

  -- Create runtime directory
  _ <- createNetworkRuntimeDir config networkId
  socketPath <- getVdeSwitchSocket config networkId

  -- Spawn vde_switch (with -tap if subnet is configured)
  let vdeBinary = qcVdeSwitchBinary config
      baseArgs = ["--sock", socketPath, "-mod", "777"]
      vdeArgs = if hasSubnet then baseArgs ++ ["-tap", tapName] else baseArgs
      (binary, args) = doas vdeBinary vdeArgs
      cp =
        (proc binary args)
          { std_in = CreatePipe -- Keep stdin open so vde_switch doesn't exit on EOF
          , std_out = CreatePipe
          , std_err = CreatePipe
          , create_group = True
          }

  logCmd binary args
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
            -- Stop dnsmasq if it's still running and clear all PIDs
            stopNetwork config pool networkId

          -- Wait for socket to become available
          waitForSocket socketPath

          -- Configure TAP interface and start dnsmasq if subnet is set
          if hasSubnet
            then do
              tapResult <- configureTapInterface subnet tapName
              case tapResult of
                Left err -> do
                  -- Roll back: stop vde_switch
                  void $ doasKill (fromIntegral pid)
                  runSqlPool (update key [M.NetworkVdeSwitchPid =. Nothing]) pool
                  pure $ Left err
                Right () -> do
                  dnsResult <- startDnsmasq config pool networkId subnet tapName
                  case dnsResult of
                    Left err -> do
                      -- Roll back: stop vde_switch
                      void $ doasKill (fromIntegral pid)
                      runSqlPool (update key [M.NetworkVdeSwitchPid =. Nothing]) pool
                      pure $ Left err
                    Right () -> pure $ Right ()
            else pure $ Right ()

-- | Stop a vde_switch process (and dnsmasq if running) for a network.
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
        -- Stop dnsmasq first if running
        forM_ (networkDnsmasqPid network) $ \dnsPid -> do
          doasKill dnsPid
          runSqlPool (update key [M.NetworkDnsmasqPid =. Nothing]) pool

        -- Stop vde_switch
        result <- doasKill vdePid
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

-- | Configure the TAP interface with an IP address from the subnet.
configureTapInterface :: Text -> String -> IO (Either Text ())
configureTapInterface subnet tapName = do
  case (,) <$> gatewayAddress subnet <*> prefixLength subnet of
    Left err -> pure $ Left $ "Subnet error: " <> err
    Right (gw, prefix) -> do
      -- ip addr add <gateway>/<prefix> dev <tapName>
      let ipAddrArgs = ["addr", "add", T.unpack gw <> "/" <> T.unpack prefix, "dev", tapName]
          (addrBin, addrArgs) = doas "ip" ipAddrArgs
          addrCp =
            (proc addrBin addrArgs)
              { std_out = CreatePipe
              , std_err = CreatePipe
              }
      logCmd addrBin addrArgs
      addrResult <- try $ createProcess addrCp
      case addrResult of
        Left (err :: SomeException) ->
          pure $ Left $ "Failed to configure TAP address: " <> T.pack (show err)
        Right (_, _, _, addrPh) -> do
          addrExit <- waitForProcess addrPh
          unless (addrExit == ExitSuccess) $
            pure ()

          -- ip link set <tapName> up
          let ipLinkArgs = ["link", "set", tapName, "up"]
              (linkBin, linkArgs) = doas "ip" ipLinkArgs
              linkCp =
                (proc linkBin linkArgs)
                  { std_out = CreatePipe
                  , std_err = CreatePipe
                  }
          logCmd linkBin linkArgs
          linkResult <- try $ createProcess linkCp
          case linkResult of
            Left (err :: SomeException) ->
              pure $ Left $ "Failed to bring up TAP interface: " <> T.pack (show err)
            Right (_, _, _, linkPh) -> do
              linkExit <- waitForProcess linkPh
              case linkExit of
                ExitSuccess -> pure $ Right ()
                ExitFailure code ->
                  pure $ Left $ "Failed to bring up TAP interface (exit code " <> T.pack (show code) <> ")"

-- | Start dnsmasq for a network's TAP interface.
startDnsmasq :: QemuConfig -> Pool SqlBackend -> Int64 -> Text -> String -> IO (Either Text ())
startDnsmasq config pool networkId subnet tapName = do
  let key = toSqlKey networkId :: M.NetworkId
  leaseFile <- getDnsmasqLeaseFile config networkId

  case (,,,) <$> gatewayAddress subnet <*> dhcpRangeStart subnet <*> dhcpRangeEnd subnet <*> subnetMask subnet of
    Left err -> pure $ Left $ "Subnet error: " <> err
    Right (gw, rangeStart, rangeEnd, mask) -> do
      let dnsBinary = qcDnsmasqBinary config
          dnsArgs =
            [ "--conf-file=/dev/null"
            , "--bind-interfaces"
            , "--listen-address=" <> T.unpack gw
            , "--except-interface=lo"
            , "--dhcp-range=" <> T.unpack rangeStart <> "," <> T.unpack rangeEnd <> "," <> T.unpack mask <> ",12h"
            , "--keep-in-foreground"
            , "--no-hosts"
            , "--dhcp-leasefile=" <> leaseFile
            ]
          (binary, args) = doas dnsBinary dnsArgs
          cp =
            (proc binary args)
              { std_in = CreatePipe
              , std_out = CreatePipe
              , std_err = CreatePipe
              , create_group = True
              }

      logCmd binary args
      result <- try $ createProcess cp
      case result of
        Left (err :: SomeException) ->
          pure $ Left $ "Failed to start dnsmasq: " <> T.pack (show err)
        Right (_, _, mStderr, ph) -> do
          mPid <- getPid ph
          case mPid of
            Nothing -> pure $ Left "Failed to get PID of dnsmasq process"
            Just pid -> do
              -- Store dnsmasq PID in database
              runSqlPool (update key [M.NetworkDnsmasqPid =. Just (fromIntegral pid)]) pool

              -- Fork background thread to monitor dnsmasq exit
              _ <- forkIO $ do
                exitCode <- waitForProcess ph
                -- Read stderr output for error diagnostics
                stderrOutput <- case mStderr of
                  Just h -> do
                    output <- try $ hGetContents h
                    case output of
                      Left (_ :: SomeException) -> pure ""
                      Right s -> pure s
                  Nothing -> pure ""
                runStdoutLoggingT $ do
                  logWarnN $
                    "dnsmasq for network " <> T.pack (show networkId) <> " exited with " <> T.pack (show exitCode)
                  unless (null stderrOutput) $
                    logWarnN $
                      "dnsmasq stderr: " <> T.pack stderrOutput
                -- Stop vde_switch if it's still running and clear all PIDs
                stopNetwork config pool networkId

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
    -- Kill dnsmasq if running
    forM_ (networkDnsmasqPid network) $ \dnsPid ->
      void $ doasKill dnsPid
    -- Kill vde_switch if running
    forM_ (networkVdeSwitchPid network) $ \vdePid ->
      void $ doasKill vdePid
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

-- | Prepend doas to a command that requires root privileges
doas :: String -> [String] -> (String, [String])
doas binary args = ("doas", binary : args)

-- | Kill a process via doas (needed for processes started with doas)
doasKill :: Int -> IO (Either Text ())
doasKill pid = do
  let (bin, args) = doas "kill" [show pid]
      cp =
        (proc bin args)
          { std_out = CreatePipe
          , std_err = CreatePipe
          }
  logCmd bin args
  result <- try $ createProcess cp
  case result of
    Left (err :: SomeException) ->
      pure $ Left $ "Failed to kill process " <> T.pack (show pid) <> ": " <> T.pack (show err)
    Right (_, _, _, ph) -> do
      exitCode <- waitForProcess ph
      case exitCode of
        ExitSuccess -> pure $ Right ()
        ExitFailure code ->
          pure $ Left $ "Failed to kill process " <> T.pack (show pid) <> " (exit code " <> T.pack (show code) <> ")"

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
