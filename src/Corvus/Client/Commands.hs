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
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever, when)
import Corvus.Client.Config (ClientConfig (..), defaultClientConfig)
import Corvus.Client.Connection
import Corvus.Client.Rpc
import Corvus.Client.Types
import Corvus.Model (EnumText (..), VmStatus (..))
import Corvus.Protocol (DriveInfo (..), NetIfInfo (..), StatusInfo (..), VmDetails (..), VmInfo (..))
import Corvus.Types (ListenAddress (..), getDefaultSocketPath)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..), close, defaultProtocol, socket)
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (..), hFlush, hPutChar, hReady, hSetBuffering, hSetEcho, stdin, stdout)
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
