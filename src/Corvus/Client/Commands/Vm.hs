{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VM command handlers for the Corvus client.
module Corvus.Client.Commands.Vm
  ( -- * VM command handlers
    handleVmCreate
  , handleVmDelete
  , handleVmAction
  , handleVmEdit

    -- * VM display/interaction
  , runRemoteViewer
  , runMonitorSession

    -- * Formatters
  , printVmInfo
  , printVmDetails
  , formatUptime
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Corvus.Client.Config (ClientConfig (..), defaultClientConfig)
import Corvus.Client.Connection
import Corvus.Client.Output (isStructured, outputError, outputOk, outputOkWith)
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Model (EnumText (..), VmStatus (..))
import Corvus.Protocol (DriveInfo (..), NetIfInfo (..), VmDetails (..), VmInfo (..))
import Data.Aeson (toJSON, (.=))
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..), close, defaultProtocol, socket)
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import System.Posix.Signals (Handler (..), installHandler, sigINT)
import System.Process (callProcess)
import Text.Printf (printf)

-- | Handle VM creation
handleVmCreate :: OutputFormat -> Connection -> Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> IO Bool
handleVmCreate fmt conn name cpuCount ramMb mDesc headless guestAgent = do
  resp <- vmCreate conn name cpuCount ramMb mDesc headless guestAgent
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (VmCreated vmId) -> do
      if isStructured fmt
        then outputOkWith fmt [("id", toJSON vmId)]
        else putStrLn $ "VM '" ++ T.unpack name ++ "' created with ID: " ++ show vmId
      pure True
    Right (VmCreateError msg) -> do
      if isStructured fmt
        then outputError fmt "create_failed" msg
        else putStrLn $ "Failed to create VM: " ++ T.unpack msg
      pure False

-- | Handle VM deletion
handleVmDelete :: OutputFormat -> Connection -> Int64 -> IO Bool
handleVmDelete fmt conn vmId = do
  resp <- vmDelete conn vmId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right VmDeleted -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn $ "VM " ++ show vmId ++ " deleted."
      pure True
    Right VmDeleteNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
        else putStrLn $ "Error: VM with ID " ++ show vmId ++ " not found."
      pure False
    Right VmDeleteRunning -> do
      if isStructured fmt
        then outputError fmt "vm_running" ("VM " <> T.pack (show vmId) <> " is running")
        else putStrLn $ "Error: VM " ++ show vmId ++ " is running. Stop it before deleting."
      pure False
    Right (VmDeleteError msg) -> do
      if isStructured fmt
        then outputError fmt "delete_failed" msg
        else putStrLn $ "Failed to delete VM: " ++ T.unpack msg
      pure False

-- | Handle VM action result
handleVmAction :: OutputFormat -> String -> Int64 -> IO (Either ConnectionError VmActionResult) -> IO Bool
handleVmAction fmt actionName vmId action = do
  resp <- action
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right VmActionNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
        else putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (VmActionInvalid currentStatus errMsg) -> do
      if isStructured fmt
        then outputError fmt "invalid_transition" errMsg
        else do
          putStrLn $ "Cannot " ++ actionName ++ " VM " ++ show vmId ++ ": " ++ T.unpack errMsg
          putStrLn $ "Current status: " ++ T.unpack (enumToText currentStatus)
      pure False
    Right (VmActionSuccess newStatus) -> do
      if isStructured fmt
        then outputOkWith fmt [("newState", toJSON newStatus)]
        else do
          putStrLn $ "VM " ++ show vmId ++ " " ++ actionName ++ ": OK"
          putStrLn $ "New status: " ++ T.unpack (enumToText newStatus)
      pure True

-- | Handle VM edit
handleVmEdit :: OutputFormat -> Connection -> Int64 -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Bool -> IO Bool
handleVmEdit fmt conn vmId mCpus mRam mDesc mHeadless mGuestAgent = do
  resp <- vmEdit conn vmId mCpus mRam mDesc mHeadless mGuestAgent
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right VmEdited -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn $ "VM " ++ show vmId ++ " updated."
      pure True
    Right VmEditNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
        else putStrLn $ "Error: VM with ID " ++ show vmId ++ " not found."
      pure False
    Right VmEditMustBeStopped -> do
      if isStructured fmt
        then outputError fmt "vm_must_be_stopped" ("VM " <> T.pack (show vmId) <> " must be stopped to edit")
        else putStrLn $ "Error: VM " ++ show vmId ++ " must be stopped to edit properties."
      pure False
    Right (VmEditError msg) -> do
      if isStructured fmt
        then outputError fmt "edit_failed" msg
        else putStrLn $ "Failed to edit VM: " ++ T.unpack msg
      pure False

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
  putStrLn $ "Console:        " ++ if vdHeadless vm then "serial (headless)" else "SPICE (graphics)"
  putStrLn $ "Guest Agent:    " ++ if vdGuestAgent vm then "enabled" else "disabled"
  putStrLn $ "Monitor Socket: " ++ T.unpack (vdMonitorSocket vm)
  if vdHeadless vm
    then putStrLn $ "Serial Socket:  " ++ T.unpack (vdSerialSocket vm)
    else putStrLn $ "SPICE Socket:   " ++ T.unpack (vdSpiceSocket vm)
  putStrLn $ "Guest Agent:    " ++ T.unpack (vdGuestAgentSocket vm)

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

      -- Install SIGINT handler that sends Ctrl+C (0x03) to the VM
      -- instead of terminating the client
      oldHandler <- installHandler sigINT (Catch (sendAll sock (BS.singleton 0x03))) Nothing

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
        result' <- try loop
        case result' of
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
        result' <- try inputLoop
        case result' of
          Left (_ :: SomeException) -> putMVar exitVar ()
          Right () -> pure ()

      -- Wait for exit signal
      takeMVar exitVar

      -- Restore SIGINT handler and terminal settings
      _ <- installHandler sigINT oldHandler Nothing
      hSetBuffering stdin LineBuffering
      hSetEcho stdin True
