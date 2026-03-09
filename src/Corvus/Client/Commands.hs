{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command execution for the Corvus client.
module Corvus.Client.Commands
  ( -- * Command execution
    runCommand,

    -- * Formatters
    formatUptime,
    printVmInfo,
    printVmDetails,
    printDiskInfo,
    printSnapshotInfo,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Corvus.Client.Config (ClientConfig (..), defaultClientConfig)
import Corvus.Client.Connection
import Corvus.Client.Rpc
import Corvus.Client.Types
import Corvus.Model (DriveFormat (..), DriveInterface (..), DriveMedia (..), EnumText (..), VmStatus (..), enumFromText)
import Corvus.Protocol (DiskImageInfo (..), DriveInfo (..), NetIfInfo (..), SnapshotInfo (..), StatusInfo (..), VmDetails (..), VmInfo (..))
import Corvus.Types (ListenAddress (..), getDefaultSocketPath)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..), close, defaultProtocol, socket)
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import System.Process (callProcess)
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
      DiskDelete diskId -> handleDiskDelete conn diskId
      DiskResize diskId newSizeMb -> handleDiskResize conn diskId newSizeMb
      DiskList -> handleDiskList conn
      DiskShow diskId -> handleDiskShow conn diskId
      DiskAttach vmId diskId ifaceStr media -> do
        case parseInterface ifaceStr of
          Left err -> do
            putStrLn $ "Error: " ++ T.unpack err
            pure False
          Right iface -> do
            case media of
              Nothing -> handleDiskAttach conn vmId diskId iface Nothing
              Just m -> case parseMedia m of
                Left err -> do
                  putStrLn $ "Error: " ++ T.unpack err
                  pure False
                Right parsedMedia -> handleDiskAttach conn vmId diskId iface (Just parsedMedia)
      DiskDetach vmId driveId -> handleDiskDetach conn vmId driveId
      -- Snapshot commands
      SnapshotCreate diskId name -> handleSnapshotCreate conn diskId name
      SnapshotDelete diskId snapshotId -> handleSnapshotDelete conn diskId snapshotId
      SnapshotRollback diskId snapshotId -> handleSnapshotRollback conn diskId snapshotId
      SnapshotMerge diskId snapshotId -> handleSnapshotMerge conn diskId snapshotId
      SnapshotList diskId -> handleSnapshotList conn diskId

  case connResult of
    Left err -> do
      putStrLn $ "Connection error: " ++ show err
      exitFailure
    Right True -> exitSuccess
    Right False -> exitFailure

-- | Run remote-viewer to connect to SPICE
runRemoteViewer :: ClientConfig -> FilePath -> IO Bool
runRemoteViewer config spiceSock = do
  let viewer = ccRemoteViewer config
      uri = "spice+unix://" ++ spiceSock
  result <- try $ callProcess viewer [uri]
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "Failed to run remote-viewer: " ++ show e
      pure False
    Right () -> pure True

-- | Run interactive HMP monitor session
-- Connects to Unix socket and relays stdin/stdout
-- Exits on Ctrl+] (ASCII 29)
runMonitorSession :: FilePath -> IO Bool
runMonitorSession sockPath = do
  result <-
    try $
      bracket
        ( do
            sock <- socket AF_UNIX Stream defaultProtocol
            NS.connect sock (SockAddrUnix sockPath)
            pure sock
        )
        close
        runSession
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "\nFailed to connect to monitor: " ++ show e
      pure False
    Right () -> do
      putStrLn "\nDisconnected from monitor."
      pure True
  where
    runSession :: Socket -> IO ()
    runSession sock = do
      -- Set terminal to raw mode
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
      hSetEcho stdin False

      -- MVar to signal exit
      exitVar <- newEmptyMVar

      -- Thread to read from socket and print to stdout
      _ <- forkIO $ do
        let loop = do
              chunk <- recv sock 4096
              if BS.null chunk
                then putMVar exitVar ()
                else do
                  BS.putStr chunk
                  hFlush stdout
                  loop
        result <- try loop
        case result of
          Left (_ :: SomeException) -> putMVar exitVar ()
          Right () -> pure ()

      -- Read from stdin and send to socket
      let inputLoop = do
            c <- getChar
            if ord c == 29 -- Ctrl+]
              then putMVar exitVar ()
              else do
                sendAll sock (BS.singleton (fromIntegral $ ord c))
                inputLoop

      -- Run input loop, catching exceptions
      _ <- forkIO $ do
        result <- try inputLoop
        case result of
          Left (_ :: SomeException) -> putMVar exitVar ()
          Right () -> pure ()

      -- Wait for exit signal
      takeMVar exitVar

      -- Restore terminal settings
      hSetBuffering stdin LineBuffering
      hSetEcho stdin True

-- | Handle VM action result
handleVmAction :: String -> Int64 -> IO (Either ConnectionError VmActionResult) -> IO Bool
handleVmAction actionName vmId action = do
  resp <- action
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right VmActionNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (VmActionInvalid currentStatus errMsg) -> do
      putStrLn $ "Cannot " ++ actionName ++ " VM " ++ show vmId ++ ": " ++ T.unpack errMsg
      putStrLn $ "Current status: " ++ T.unpack (enumToText currentStatus)
      pure False
    Right (VmActionSuccess newStatus) -> do
      putStrLn $ "VM " ++ show vmId ++ " " ++ actionName ++ ": OK"
      putStrLn $ "New status: " ++ T.unpack (enumToText newStatus)
      pure True

-- | Print VM info in table format
printVmInfo :: VmInfo -> IO ()
printVmInfo vm =
  putStrLn $
    printf
      "%-6d %-20s %-12s %5d %8d"
      (viId vm)
      (T.unpack $ viName vm)
      (T.unpack $ enumToText $ viStatus vm)
      (viCpuCount vm)
      (viRamMb vm)

-- | Print full VM details
printVmDetails :: VmDetails -> IO ()
printVmDetails vm = do
  putStrLn $ "VM ID:          " ++ show (vdId vm)
  putStrLn $ "Name:           " ++ T.unpack (vdName vm)
  putStrLn $ "Created:        " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (vdCreatedAt vm)
  putStrLn $ "Status:         " ++ T.unpack (enumToText $ vdStatus vm)
  putStrLn $ "CPUs:           " ++ show (vdCpuCount vm)
  putStrLn $ "RAM (MB):       " ++ show (vdRamMb vm)
  putStrLn $ "Description:    " ++ maybe "(none)" T.unpack (vdDescription vm)
  putStrLn $ "Monitor Socket: " ++ T.unpack (vdMonitorSocket vm)
  putStrLn $ "SPICE Socket:   " ++ T.unpack (vdSpiceSocket vm)

  putStrLn ""
  putStrLn "Drives:"
  if null (vdDrives vm)
    then putStrLn "  (none)"
    else mapM_ printDrive (vdDrives vm)

  putStrLn ""
  putStrLn "Network Interfaces:"
  if null (vdNetIfs vm)
    then putStrLn "  (none)"
    else mapM_ printNetIf (vdNetIfs vm)
  where
    printDrive d = do
      putStrLn $ "  - ID: " ++ show (diId d)
      putStrLn $ "    Interface: " ++ T.unpack (enumToText $ diInterface d)
      putStrLn $ "    Path: " ++ T.unpack (diFilePath d)
      putStrLn $ "    Format: " ++ T.unpack (enumToText $ diFormat d)
      putStrLn $ "    Read-only: " ++ show (diReadOnly d)
      putStrLn $ "    Cache: " ++ T.unpack (enumToText $ diCacheType d)
      putStrLn $ "    Discard: " ++ show (diDiscard d)

    printNetIf n = do
      putStrLn $ "  - ID: " ++ show (niId n)
      putStrLn $ "    Type: " ++ T.unpack (enumToText $ niType n)
      putStrLn $ "    Host Device: " ++ T.unpack (niHostDevice n)
      putStrLn $ "    MAC: " ++ T.unpack (niMacAddress n)

-- | Format uptime in human-readable format
formatUptime :: Int -> String
formatUptime secs
  | secs < 60 = show secs ++ "s"
  | secs < 3600 = show (secs `div` 60) ++ "m " ++ show (secs `mod` 60) ++ "s"
  | otherwise =
      show (secs `div` 3600)
        ++ "h "
        ++ show ((secs `mod` 3600) `div` 60)
        ++ "m"

--------------------------------------------------------------------------------
-- Disk Command Handlers
--------------------------------------------------------------------------------

-- | Parse format string to DriveFormat
parseFormat :: Text -> Either Text DriveFormat
parseFormat = enumFromText

-- | Parse interface string to DriveInterface
parseInterface :: Text -> Either Text DriveInterface
parseInterface = enumFromText

-- | Parse media string to DriveMedia
parseMedia :: Text -> Either Text DriveMedia
parseMedia = enumFromText

-- | Handle disk create command
handleDiskCreate :: Connection -> Text -> DriveFormat -> Int64 -> IO Bool
handleDiskCreate conn name format sizeMb = do
  resp <- diskCreate conn name format sizeMb
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskCreated diskId) -> do
      putStrLn $ "Disk image created with ID: " ++ show diskId
      pure True
    Right (DiskError msg) -> do
      putStrLn $ "Error creating disk: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk delete command
handleDiskDelete :: Connection -> Int64 -> IO Bool
handleDiskDelete conn diskId = do
  resp <- diskDelete conn diskId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right DiskOk -> do
      putStrLn "Disk image deleted."
      pure True
    Right DiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right (DiskInUse vmIds) -> do
      putStrLn $ "Disk is attached to VMs: " ++ show vmIds
      putStrLn "Detach the disk first before deleting."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk resize command
handleDiskResize :: Connection -> Int64 -> Int64 -> IO Bool
handleDiskResize conn diskId newSizeMb = do
  resp <- diskResize conn diskId newSizeMb
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right DiskOk -> do
      putStrLn $ "Disk resized to " ++ show newSizeMb ++ " MB."
      pure True
    Right DiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right VmMustBeStopped -> do
      putStrLn "Cannot resize disk while VM is running. Stop the VM first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk list command
handleDiskList :: Connection -> IO Bool
handleDiskList conn = do
  resp <- diskList conn
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskListResult disks) -> do
      if null disks
        then putStrLn "No disk images found."
        else do
          putStrLn $
            printf
              "%-6s %-20s %-8s %10s %-20s"
              ("ID" :: String)
              ("NAME" :: String)
              ("FORMAT" :: String)
              ("SIZE_MB" :: String)
              ("ATTACHED_TO" :: String)
          putStrLn $ replicate 70 '-'
          mapM_ printDiskInfo disks
      pure True
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk show command
handleDiskShow :: Connection -> Int64 -> IO Bool
handleDiskShow conn diskId = do
  resp <- diskShow conn diskId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskInfo info) -> do
      printDiskDetails info
      pure True
    Right DiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk attach command
handleDiskAttach :: Connection -> Int64 -> Int64 -> DriveInterface -> Maybe DriveMedia -> IO Bool
handleDiskAttach conn vmId diskId iface media = do
  resp <- diskAttach conn vmId diskId iface media
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (DriveAttached driveId) -> do
      putStrLn $ "Disk attached. Drive ID: " ++ show driveId
      pure True
    Right DiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right DiskVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (DiskError msg) -> do
      putStrLn $ "Error attaching disk: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk detach command
handleDiskDetach :: Connection -> Int64 -> Int64 -> IO Bool
handleDiskDetach conn vmId driveId = do
  resp <- diskDetach conn vmId driveId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right DiskOk -> do
      putStrLn "Disk detached."
      pure True
    Right DriveNotFound -> do
      putStrLn $ "Drive with ID " ++ show driveId ++ " not found."
      pure False
    Right DiskVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (DiskError msg) -> do
      putStrLn $ "Error detaching disk: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

--------------------------------------------------------------------------------
-- Snapshot Command Handlers
--------------------------------------------------------------------------------

-- | Handle snapshot create command
handleSnapshotCreate :: Connection -> Int64 -> Text -> IO Bool
handleSnapshotCreate conn diskId name = do
  resp <- snapshotCreate conn diskId name
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (SnapshotCreated snapId) -> do
      putStrLn $ "Snapshot created with ID: " ++ show snapId
      pure True
    Right SnapshotDiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right (SnapshotFormatNotSupported msg) -> do
      putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right SnapshotVmMustBeStopped -> do
      putStrLn "Cannot create snapshot while VM is running. Stop the VM first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot delete command
handleSnapshotDelete :: Connection -> Int64 -> Int64 -> IO Bool
handleSnapshotDelete conn diskId snapshotId = do
  resp <- snapshotDelete conn diskId snapshotId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right SnapshotOk -> do
      putStrLn "Snapshot deleted."
      pure True
    Right SnapshotNotFound -> do
      putStrLn $ "Snapshot with ID " ++ show snapshotId ++ " not found."
      pure False
    Right SnapshotDiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      putStrLn "Cannot delete snapshot while VM is running. Stop the VM first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot rollback command
handleSnapshotRollback :: Connection -> Int64 -> Int64 -> IO Bool
handleSnapshotRollback conn diskId snapshotId = do
  resp <- snapshotRollback conn diskId snapshotId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right SnapshotOk -> do
      putStrLn "Rollback complete."
      pure True
    Right SnapshotNotFound -> do
      putStrLn $ "Snapshot with ID " ++ show snapshotId ++ " not found."
      pure False
    Right SnapshotDiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      putStrLn "Cannot rollback while VM is running. Stop the VM first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot merge command
handleSnapshotMerge :: Connection -> Int64 -> Int64 -> IO Bool
handleSnapshotMerge conn diskId snapshotId = do
  resp <- snapshotMerge conn diskId snapshotId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right SnapshotOk -> do
      putStrLn "Snapshot merged."
      pure True
    Right SnapshotNotFound -> do
      putStrLn $ "Snapshot with ID " ++ show snapshotId ++ " not found."
      pure False
    Right SnapshotDiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      putStrLn "Cannot merge while VM is running. Stop the VM first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot list command
handleSnapshotList :: Connection -> Int64 -> IO Bool
handleSnapshotList conn diskId = do
  resp <- snapshotList conn diskId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (SnapshotListResult snaps) -> do
      if null snaps
        then putStrLn "No snapshots found."
        else do
          putStrLn $
            printf
              "%-6s %-30s %-20s %10s"
              ("ID" :: String)
              ("NAME" :: String)
              ("CREATED" :: String)
              ("SIZE_MB" :: String)
          putStrLn $ replicate 70 '-'
          mapM_ printSnapshotInfo snaps
      pure True
    Right SnapshotDiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

--------------------------------------------------------------------------------
-- Disk/Snapshot Printers
--------------------------------------------------------------------------------

-- | Print disk image info in table format
printDiskInfo :: DiskImageInfo -> IO ()
printDiskInfo d =
  putStrLn $
    printf
      "%-6d %-20s %-8s %10s %-20s"
      (diiId d)
      (T.unpack $ diiName d)
      (T.unpack $ enumToText $ diiFormat d)
      (maybe "-" show $ diiSizeMb d)
      (if null (diiAttachedTo d) then "-" else show (diiAttachedTo d))

-- | Print disk image details
printDiskDetails :: DiskImageInfo -> IO ()
printDiskDetails d = do
  putStrLn $ "Disk ID:     " ++ show (diiId d)
  putStrLn $ "Name:        " ++ T.unpack (diiName d)
  putStrLn $ "File Path:   " ++ T.unpack (diiFilePath d)
  putStrLn $ "Format:      " ++ T.unpack (enumToText $ diiFormat d)
  putStrLn $ "Size (MB):   " ++ maybe "(unknown)" show (diiSizeMb d)
  putStrLn $ "Created:     " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (diiCreatedAt d)
  putStrLn $ "Attached to: " ++ if null (diiAttachedTo d) then "(none)" else show (diiAttachedTo d)

-- | Print snapshot info in table format
printSnapshotInfo :: SnapshotInfo -> IO ()
printSnapshotInfo s =
  putStrLn $
    printf
      "%-6d %-30s %-20s %10s"
      (sniId s)
      (T.unpack $ sniName s)
      (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (sniCreatedAt s))
      (maybe "-" show $ sniSizeMb s)
