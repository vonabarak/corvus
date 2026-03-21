{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command execution for the Corvus client.
-- This module dispatches CLI commands to domain-specific handler modules.
module Corvus.Client.Commands
  ( -- * Command execution
    runCommand,

    -- * Re-exported formatters
    formatUptime,
    printVmInfo,
    printVmDetails,
    printDiskInfo,
    printSnapshotInfo,
    printSshKeyInfo,
    printTemplateVmInfo,
    printTemplateDetails,
  )
where

import Corvus.Client.Commands.Disk
import Corvus.Client.Commands.NetIf
import Corvus.Client.Commands.SharedDir
import Corvus.Client.Commands.SshKey
import Corvus.Client.Commands.Template
import Corvus.Client.Commands.Vm
import Corvus.Client.Config (defaultClientConfig)
import Corvus.Client.Connection
import Corvus.Client.Rpc
import Corvus.Client.Types
import Corvus.Model (EnumText (..), VmStatus (..))
import Corvus.Protocol (StatusInfo (..), VmDetails (..), VmInfo (..))
import Corvus.Types (ListenAddress (..), getDefaultSocketPath)
import qualified Data.Text as T
import System.Exit (exitFailure, exitSuccess)
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
  addr <- getListenAddress opts
  connResult <- withConnection addr $ \conn ->
    case optCommand opts of
      Ping -> do
        resp <- sendPing conn
        case resp of
          Left err -> do
            putStrLn $ "Error: " ++ show err
            pure False
          Right () -> do
            putStrLn "pong"
            pure True
      Status -> do
        resp <- getStatus conn
        case resp of
          Left err -> do
            putStrLn $ "Error: " ++ show err
            pure False
          Right st -> do
            putStrLn $ "Uptime:      " ++ formatUptime (siUptime st)
            putStrLn $ "Connections: " ++ show (siConnections st)
            putStrLn $ "Version:     " ++ T.unpack (siVersion st)
            pure True
      Shutdown -> do
        resp <- requestShutdown conn
        case resp of
          Left err -> do
            putStrLn $ "Error: " ++ show err
            pure False
          Right True -> do
            putStrLn "Shutdown acknowledged"
            pure True
          Right False -> do
            putStrLn "Shutdown not acknowledged"
            pure False
      VmList -> do
        resp <- listVms conn
        case resp of
          Left err -> do
            putStrLn $ "Error: " ++ show err
            pure False
          Right vms -> do
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
            putStrLn $ "Error: " ++ show err
            pure False
          Right Nothing -> do
            putStrLn $ "VM with ID " ++ show vmId ++ " not found."
            pure False
          Right (Just details) -> do
            printVmDetails details
            pure True
      VmCreate name cpuCount ramMb mDesc -> handleVmCreate conn name cpuCount ramMb mDesc
      VmDelete vmId -> handleVmDelete conn vmId
      VmStart vmId -> handleVmAction "start" vmId (vmStart conn vmId)
      VmStop vmId -> handleVmAction "stop" vmId (vmStop conn vmId)
      VmPause vmId -> handleVmAction "pause" vmId (vmPause conn vmId)
      VmReset vmId -> handleVmAction "reset" vmId (vmReset conn vmId)
      VmView vmId -> do
        resp <- showVm conn vmId
        case resp of
          Left err -> do
            putStrLn $ "Error: " ++ show err
            pure False
          Right Nothing -> do
            putStrLn $ "VM with ID " ++ show vmId ++ " not found."
            pure False
          Right (Just details) -> do
            if vdStatus details /= VmRunning
              then do
                putStrLn $ "Error: VM '" ++ T.unpack (vdName details) ++ "' is not running."
                putStrLn $ "Current status: " ++ T.unpack (enumToText $ vdStatus details)
                pure False
              else do
                let spiceSock = T.unpack (vdSpiceSocket details)
                putStrLn $ "Connecting to VM '" ++ T.unpack (vdName details) ++ "' via SPICE..."
                runRemoteViewer defaultClientConfig spiceSock
      VmMonitor vmId -> do
        resp <- showVm conn vmId
        case resp of
          Left err -> do
            putStrLn $ "Error: " ++ show err
            pure False
          Right Nothing -> do
            putStrLn $ "VM with ID " ++ show vmId ++ " not found."
            pure False
          Right (Just details) -> do
            if vdStatus details /= VmRunning
              then do
                putStrLn $ "Error: VM '" ++ T.unpack (vdName details) ++ "' is not running."
                putStrLn $ "Current status: " ++ T.unpack (enumToText $ vdStatus details)
                pure False
              else do
                let monitorSock = T.unpack (vdMonitorSocket details)
                putStrLn $ "Connecting to VM '" ++ T.unpack (vdName details) ++ "' HMP monitor..."
                putStrLn "Press Ctrl+] to exit."
                putStrLn ""
                runMonitorSession monitorSock
      -- Disk commands
      DiskCreate name formatStr sizeMb -> do
        case parseFormat formatStr of
          Left err -> do
            putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right format -> handleDiskCreate conn name format sizeMb
      DiskCreateOverlay name baseDiskId -> handleDiskCreateOverlay conn name baseDiskId
      DiskImport name path mFormatStr -> handleDiskImport conn name path mFormatStr
      DiskDelete diskId -> handleDiskDelete conn diskId
      DiskResize diskId newSizeMb -> handleDiskResize conn diskId newSizeMb
      DiskList -> handleDiskList conn
      DiskShow diskId -> handleDiskShow conn diskId
      DiskClone name baseDiskId optionalPath -> handleDiskClone conn name baseDiskId optionalPath
      DiskAttach vmId diskId ifaceStr media readOnly discard cacheStr -> do
        case parseInterface ifaceStr of
          Left err -> do
            putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right iface -> do
            case parseCacheType cacheStr of
              Left err -> do
                putStrLn $ "Error: " ++ T.unpack err
                pure False
              Right cache -> do
                case media of
                  Nothing -> handleDiskAttach conn vmId diskId iface Nothing readOnly discard cache
                  Just m -> case parseMedia m of
                    Left err -> do
                      putStrLn $ "Error: " ++ T.unpack err
                      pure False
                    Right parsedMedia -> handleDiskAttach conn vmId diskId iface (Just parsedMedia) readOnly discard cache
      DiskDetach vmId driveId -> handleDiskDetach conn vmId driveId
      -- Shared directory commands
      SharedDirAdd vmId path tag cacheStr readOnly -> do
        case parseSharedDirCache cacheStr of
          Left err -> do
            putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right cache -> handleSharedDirAdd conn vmId path tag cache readOnly
      SharedDirRemove vmId sharedDirId -> handleSharedDirRemove conn vmId sharedDirId
      SharedDirList vmId -> handleSharedDirList conn vmId
      -- Network interface commands
      NetIfAdd vmId ifaceTypeStr hostDevice macAddress -> do
        case parseNetInterfaceType ifaceTypeStr of
          Left err -> do
            putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right ifaceType -> handleNetIfAdd conn vmId ifaceType hostDevice macAddress
      NetIfRemove vmId netIfId -> handleNetIfRemove conn vmId netIfId
      NetIfList vmId -> handleNetIfList conn vmId
      -- Snapshot commands
      SnapshotCreate diskId name -> handleSnapshotCreate conn diskId name
      SnapshotDelete diskId snapshotId -> handleSnapshotDelete conn diskId snapshotId
      SnapshotRollback diskId snapshotId -> handleSnapshotRollback conn diskId snapshotId
      SnapshotMerge diskId snapshotId -> handleSnapshotMerge conn diskId snapshotId
      SnapshotList diskId -> handleSnapshotList conn diskId
      -- SSH key commands
      SshKeyCreate name publicKey -> handleSshKeyCreate conn name publicKey
      SshKeyDelete keyId -> handleSshKeyDelete conn keyId
      SshKeyList -> handleSshKeyList conn
      SshKeyAttach vmId keyId -> handleSshKeyAttach conn vmId keyId
      SshKeyDetach vmId keyId -> handleSshKeyDetach conn vmId keyId
      SshKeyListForVm vmId -> handleSshKeyListForVm conn vmId
      TemplateCreate path -> handleTemplateCreate conn path
      TemplateDelete tid -> handleTemplateDelete conn tid
      TemplateList -> handleTemplateList conn
      TemplateShow tid -> handleTemplateShow conn tid
      TemplateInstantiate tid name -> handleTemplateInstantiate conn tid name

  case connResult of
    Left err -> do
      putStrLn $ "Connection error: " ++ show err
      exitFailure
    Right True -> exitSuccess
    Right False -> exitFailure
