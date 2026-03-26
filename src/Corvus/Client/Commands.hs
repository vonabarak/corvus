{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command execution for the Corvus client.
-- This module dispatches CLI commands to domain-specific handler modules.
module Corvus.Client.Commands
  ( -- * Command execution
    runCommand

    -- * Re-exported formatters
  , formatUptime
  , printVmInfo
  , printVmDetails
  , printDiskInfo
  , printSnapshotInfo
  , printSshKeyInfo
  , printTemplateVmInfo
  , printTemplateDetails
  )
where

import Corvus.Client.Commands.Disk
import Corvus.Client.Commands.GuestExec
import Corvus.Client.Commands.NetIf
import Corvus.Client.Commands.Network
import Corvus.Client.Commands.SharedDir
import Corvus.Client.Commands.SshKey
import Corvus.Client.Commands.Template
import Corvus.Client.Commands.Vm
import Corvus.Client.Config (defaultClientConfig)
import Corvus.Client.Connection
import Corvus.Client.Output
import Corvus.Client.Rpc
import Corvus.Client.Types
import Corvus.Model (EnumText (..), VmStatus (..))
import Corvus.Protocol (StatusInfo (..), VmDetails (..), VmInfo (..))
import Corvus.Types (ListenAddress (..), getDefaultSocketPath)
import Data.Aeson (object, toJSON, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure, exitSuccess)
import System.Posix.Signals (Handler (..), installHandler, sigINT)
import Text.Printf (printf)

-- | Get the listen address from options
getListenAddress :: Options -> IO ListenAddress
getListenAddress opts
  | optTcp opts = pure $ TcpAddress (optHost opts) (optPort opts)
  | otherwise = case optSocket opts of
      Just path -> pure $ UnixAddress path
      Nothing -> UnixAddress <$> getDefaultSocketPath

-- | Execute the selected command
runCommand :: Options -> IO ()
runCommand opts = do
  let fmt = optOutput opts
  addr <- getListenAddress opts
  connResult <- withConnection addr $ \conn ->
    case optCommand opts of
      Ping -> do
        resp <- sendPing conn
        case resp of
          Left err -> do
            if isStructured fmt
              then outputError fmt "rpc_error" (T.pack $ show err)
              else putStrLn $ "Error: " ++ show err
            pure False
          Right () -> do
            if isStructured fmt
              then outputOk fmt
              else putStrLn "pong"
            pure True
      Status -> do
        resp <- getStatus conn
        case resp of
          Left err -> do
            if isStructured fmt
              then outputError fmt "rpc_error" (T.pack $ show err)
              else putStrLn $ "Error: " ++ show err
            pure False
          Right st -> do
            if isStructured fmt
              then outputResult fmt st
              else do
                putStrLn $ "Uptime:      " ++ formatUptime (siUptime st)
                putStrLn $ "Connections: " ++ show (siConnections st)
                putStrLn $ "Version:     " ++ T.unpack (siVersion st)
            pure True
      Shutdown -> do
        resp <- requestShutdown conn
        case resp of
          Left err -> do
            if isStructured fmt
              then outputError fmt "rpc_error" (T.pack $ show err)
              else putStrLn $ "Error: " ++ show err
            pure False
          Right True -> do
            if isStructured fmt
              then outputOk fmt
              else putStrLn "Shutdown acknowledged"
            pure True
          Right False -> do
            if isStructured fmt
              then outputError fmt "shutdown_rejected" "Shutdown not acknowledged"
              else putStrLn "Shutdown not acknowledged"
            pure False
      VmList -> do
        resp <- listVms conn
        case resp of
          Left err -> do
            if isStructured fmt
              then outputError fmt "rpc_error" (T.pack $ show err)
              else putStrLn $ "Error: " ++ show err
            pure False
          Right vms -> do
            if isStructured fmt
              then outputResult fmt vms
              else do
                if null vms
                  then putStrLn "No VMs found."
                  else do
                    putStrLn $
                      printf
                        "%-6s %-20s %-12s %5s %8s"
                        ("ID" :: String)
                        ("NAME" :: String)
                        ("STATUS" :: String)
                        ("CPUS" :: String)
                        ("RAM_MB" :: String)
                    putStrLn $ replicate 55 '-'
                    mapM_ printVmInfo vms
            pure True
      VmShow vmId -> do
        resp <- showVm conn vmId
        case resp of
          Left err -> do
            if isStructured fmt
              then outputError fmt "rpc_error" (T.pack $ show err)
              else putStrLn $ "Error: " ++ show err
            pure False
          Right Nothing -> do
            if isStructured fmt
              then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
              else putStrLn $ "VM with ID " ++ show vmId ++ " not found."
            pure False
          Right (Just details) -> do
            if isStructured fmt
              then outputResult fmt details
              else printVmDetails details
            pure True
      VmCreate name cpuCount ramMb mDesc headless ga -> handleVmCreate fmt conn name cpuCount ramMb mDesc headless ga
      VmDelete vmId -> handleVmDelete fmt conn vmId
      VmStart vmId -> handleVmAction fmt "start" vmId (vmStart conn vmId)
      VmStop vmId -> handleVmAction fmt "stop" vmId (vmStop conn vmId)
      VmPause vmId -> handleVmAction fmt "pause" vmId (vmPause conn vmId)
      VmReset vmId -> handleVmAction fmt "reset" vmId (vmReset conn vmId)
      VmEdit vmId mCpus mRam mDesc mHeadless mGa -> handleVmEdit fmt conn vmId mCpus mRam mDesc mHeadless mGa
      VmExec vmId cmd -> handleVmExec fmt conn vmId cmd
      VmView vmId -> do
        resp <- showVm conn vmId
        case resp of
          Left err -> do
            if isStructured fmt
              then outputError fmt "rpc_error" (T.pack $ show err)
              else putStrLn $ "Error: " ++ show err
            pure False
          Right Nothing -> do
            if isStructured fmt
              then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
              else putStrLn $ "VM with ID " ++ show vmId ++ " not found."
            pure False
          Right (Just details) -> do
            if vdStatus details /= VmRunning
              then do
                if isStructured fmt
                  then outputError fmt "vm_not_running" ("VM '" <> vdName details <> "' is not running")
                  else do
                    putStrLn $ "Error: VM '" ++ T.unpack (vdName details) ++ "' is not running."
                    putStrLn $ "Current status: " ++ T.unpack (enumToText $ vdStatus details)
                pure False
              else do
                if vdHeadless details
                  then do
                    let serialSock = T.unpack (vdSerialSocket details)
                    if isStructured fmt
                      then outputValue fmt (object ["serialSocket" .= vdSerialSocket details])
                      else do
                        putStrLn $ "Connecting to VM '" ++ T.unpack (vdName details) ++ "' serial console..."
                        putStrLn "Press Ctrl+] to exit."
                        putStrLn ""
                        _ <- runMonitorSession serialSock
                        pure ()
                  else do
                    let spiceSock = T.unpack (vdSpiceSocket details)
                    if isStructured fmt
                      then outputValue fmt (object ["spiceSocket" .= vdSpiceSocket details])
                      else withIgnoredSigINT $ do
                        putStrLn $ "Connecting to VM '" ++ T.unpack (vdName details) ++ "' via SPICE..."
                        _ <- runRemoteViewer defaultClientConfig spiceSock
                        pure ()
                pure True
      VmMonitor vmId -> do
        resp <- showVm conn vmId
        case resp of
          Left err -> do
            if isStructured fmt
              then outputError fmt "rpc_error" (T.pack $ show err)
              else putStrLn $ "Error: " ++ show err
            pure False
          Right Nothing -> do
            if isStructured fmt
              then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
              else putStrLn $ "VM with ID " ++ show vmId ++ " not found."
            pure False
          Right (Just details) -> do
            if vdStatus details /= VmRunning
              then do
                if isStructured fmt
                  then outputError fmt "vm_not_running" ("VM '" <> vdName details <> "' is not running")
                  else do
                    putStrLn $ "Error: VM '" ++ T.unpack (vdName details) ++ "' is not running."
                    putStrLn $ "Current status: " ++ T.unpack (enumToText $ vdStatus details)
                pure False
              else do
                let monitorSock = T.unpack (vdMonitorSocket details)
                if isStructured fmt
                  then outputValue fmt (object ["monitorSocket" .= vdMonitorSocket details])
                  else do
                    putStrLn $ "Connecting to VM '" ++ T.unpack (vdName details) ++ "' HMP monitor..."
                    putStrLn "Press Ctrl+] to exit."
                    putStrLn ""
                    runMonitorSession monitorSock
                    pure ()
                pure True
      -- Disk commands
      DiskCreate name formatStr sizeMb -> do
        case parseFormat formatStr of
          Left err -> do
            if isStructured fmt
              then outputError fmt "invalid_format" err
              else putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right format -> handleDiskCreate fmt conn name format sizeMb
      DiskCreateOverlay name baseDiskId -> handleDiskCreateOverlay fmt conn name baseDiskId
      DiskImport name path mFormatStr -> handleDiskImport fmt conn name path mFormatStr
      DiskDelete diskId -> handleDiskDelete fmt conn diskId
      DiskResize diskId newSizeMb -> handleDiskResize fmt conn diskId newSizeMb
      DiskList -> handleDiskList fmt conn
      DiskShow diskId -> handleDiskShow fmt conn diskId
      DiskClone name baseDiskId optionalPath -> handleDiskClone fmt conn name baseDiskId optionalPath
      DiskAttach vmId diskId ifaceStr media readOnly discard cacheStr -> do
        case parseInterface ifaceStr of
          Left err -> do
            if isStructured fmt
              then outputError fmt "invalid_interface" err
              else putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right iface -> do
            case parseCacheType cacheStr of
              Left err -> do
                if isStructured fmt
                  then outputError fmt "invalid_cache_type" err
                  else putStrLn $ "Error: " ++ T.unpack err
                pure False
              Right cache -> do
                case media of
                  Nothing -> handleDiskAttach fmt conn vmId diskId iface Nothing readOnly discard cache
                  Just m -> case parseMedia m of
                    Left err -> do
                      if isStructured fmt
                        then outputError fmt "invalid_media" err
                        else putStrLn $ "Error: " ++ T.unpack err
                      pure False
                    Right parsedMedia -> handleDiskAttach fmt conn vmId diskId iface (Just parsedMedia) readOnly discard cache
      DiskDetach vmId driveId -> handleDiskDetach fmt conn vmId driveId
      -- Shared directory commands
      SharedDirAdd vmId path tag cacheStr readOnly -> do
        case parseSharedDirCache cacheStr of
          Left err -> do
            if isStructured fmt
              then outputError fmt "invalid_cache" err
              else putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right cache -> handleSharedDirAdd fmt conn vmId path tag cache readOnly
      SharedDirRemove vmId sharedDirId -> handleSharedDirRemove fmt conn vmId sharedDirId
      SharedDirList vmId -> handleSharedDirList fmt conn vmId
      -- Network interface commands
      NetIfAdd vmId ifaceTypeStr hostDevice macAddress mNetworkId -> do
        case parseNetInterfaceType ifaceTypeStr of
          Left err -> do
            if isStructured fmt
              then outputError fmt "invalid_interface_type" err
              else putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right ifaceType -> handleNetIfAdd fmt conn vmId ifaceType hostDevice macAddress mNetworkId
      NetIfRemove vmId netIfId -> handleNetIfRemove fmt conn vmId netIfId
      NetIfList vmId -> handleNetIfList fmt conn vmId
      -- Snapshot commands
      SnapshotCreate diskId name -> handleSnapshotCreate fmt conn diskId name
      SnapshotDelete diskId snapshotId -> handleSnapshotDelete fmt conn diskId snapshotId
      SnapshotRollback diskId snapshotId -> handleSnapshotRollback fmt conn diskId snapshotId
      SnapshotMerge diskId snapshotId -> handleSnapshotMerge fmt conn diskId snapshotId
      SnapshotList diskId -> handleSnapshotList fmt conn diskId
      -- SSH key commands
      SshKeyCreate name publicKey -> handleSshKeyCreate fmt conn name publicKey
      SshKeyDelete keyId -> handleSshKeyDelete fmt conn keyId
      SshKeyList -> handleSshKeyList fmt conn
      SshKeyAttach vmId keyId -> handleSshKeyAttach fmt conn vmId keyId
      SshKeyDetach vmId keyId -> handleSshKeyDetach fmt conn vmId keyId
      SshKeyListForVm vmId -> handleSshKeyListForVm fmt conn vmId
      TemplateCreate path -> handleTemplateCreate fmt conn path
      TemplateDelete tid -> handleTemplateDelete fmt conn tid
      TemplateList -> handleTemplateList fmt conn
      TemplateShow tid -> handleTemplateShow fmt conn tid
      TemplateInstantiate tid name -> handleTemplateInstantiate fmt conn tid name
      -- Network commands
      NetworkCreate name subnet -> handleNetworkCreate fmt conn name subnet
      NetworkDelete nwId -> handleNetworkDelete fmt conn nwId
      NetworkStart nwId -> handleNetworkStart fmt conn nwId
      NetworkStop nwId force -> handleNetworkStop fmt conn nwId force
      NetworkList -> handleNetworkList fmt conn
      NetworkShow nwId -> handleNetworkShow fmt conn nwId

  case connResult of
    Left err -> do
      if isStructured fmt
        then outputError fmt "connection_error" (T.pack $ show err)
        else putStrLn $ "Connection error: " ++ show err
      exitFailure
    Right True -> exitSuccess
    Right False -> exitFailure

-- | Run an action with SIGINT ignored, restoring the previous handler afterwards.
-- This lets Ctrl+C pass through to the child process (VM) instead of killing the client.
withIgnoredSigINT :: IO a -> IO a
withIgnoredSigINT action = do
  oldHandler <- installHandler sigINT Ignore Nothing
  result <- action
  _ <- installHandler sigINT oldHandler Nothing
  pure result
