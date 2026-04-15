{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VM command handlers for the Corvus client.
module Corvus.Client.Commands.Vm
  ( -- * VM command handlers
    handleVmCreate
  , handleVmDelete
  , handleVmAction
  , handleVmStart
  , handleVmStop
  , handleVmEdit

    -- * Polling
  , waitForVmStatus

    -- * VM display/interaction
  , runRemoteViewer
  , runMonitorSession
  , runRawTerminalSession

    -- * Formatters
  , printVmInfo
  , printVmDetails
  , formatUptime
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, when)
import Corvus.Client.Config (ClientConfig (..), defaultClientConfig)
import Corvus.Client.Connection
import Corvus.Client.Output (emitError, emitOk, emitOkWith, emitRpcError, isStructured, printField, tableFormat)
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..), WaitOptions (..))
import Corvus.Model (EnumText (..), VmStatus (..))
import Corvus.Protocol (DriveInfo (..), NetIfInfo (..), VmDetails (..), VmInfo (..))
import Data.Aeson (toJSON, (.=))
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..), close, defaultProtocol, socket)
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import System.IO (BufferMode (..), hFlush, hPutStr, hSetBinaryMode, hSetBuffering, stderr, stdin, stdout)
import System.Posix.IO (stdInput)
import System.Posix.Terminal
import System.Process (callProcess)
import Text.Printf (printf)

-- | Handle VM creation
handleVmCreate :: OutputFormat -> Connection -> Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> Bool -> Bool -> IO Bool
handleVmCreate fmt conn name cpuCount ramMb mDesc headless guestAgent cloudInit autostart = do
  resp <- vmCreate conn name cpuCount ramMb mDesc headless guestAgent cloudInit autostart
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (VmCreated vmId) -> do
      emitOkWith fmt [("id", toJSON vmId)] $
        putStrLn $
          "VM '" ++ T.unpack name ++ "' created with ID: " ++ show vmId
      pure True
    Right (VmCreateError msg) -> do
      emitError fmt "create_failed" msg $
        putStrLn $
          "Failed to create VM: " ++ T.unpack msg
      pure False

-- | Handle VM deletion
handleVmDelete :: OutputFormat -> Connection -> Text -> Bool -> IO Bool
handleVmDelete fmt conn vmRef deleteDisks = do
  resp <- vmDelete conn vmRef deleteDisks
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right VmDeleted -> do
      emitOk fmt $ putStrLn $ "VM '" ++ T.unpack vmRef ++ "' deleted."
      pure True
    Right VmDeleteNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "Error: VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right VmDeleteRunning -> do
      emitError fmt "vm_running" ("VM '" <> vmRef <> "' is running") $
        putStrLn $
          "Error: VM '" ++ T.unpack vmRef ++ "' is running. Stop it before deleting."
      pure False
    Right (VmDeleteError msg) -> do
      emitError fmt "delete_failed" msg $
        putStrLn $
          "Failed to delete VM: " ++ T.unpack msg
      pure False

-- | Handle VM action result
handleVmAction :: OutputFormat -> String -> Text -> IO (Either ConnectionError VmActionResult) -> IO Bool
handleVmAction fmt actionName vmRef action = do
  resp <- action
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right VmActionNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right (VmActionInvalid currentStatus errMsg) -> do
      emitError fmt "invalid_transition" errMsg $ do
        putStrLn $ "Cannot " ++ actionName ++ " VM '" ++ T.unpack vmRef ++ "': " ++ T.unpack errMsg
        putStrLn $ "Current status: " ++ T.unpack (enumToText currentStatus)
      pure False
    Right (VmActionSuccess newStatus) -> do
      emitOkWith fmt [("newState", toJSON newStatus)] $ do
        putStrLn $ "VM '" ++ T.unpack vmRef ++ "' " ++ actionName ++ ": OK"
        putStrLn $ "New status: " ++ T.unpack (enumToText newStatus)
      pure True

-- | Handle VM stop with optional --wait polling
-- | Handle VM start command. With --wait, the server blocks until VmRunning.
handleVmStart :: OutputFormat -> Connection -> Text -> WaitOptions -> IO Bool
handleVmStart fmt conn vmRef waitOpts = do
  let wait = woWait waitOpts
  unless (isStructured fmt) $
    when wait $
      putStrLn $
        "Starting VM '" ++ T.unpack vmRef ++ "' and waiting for it to become running..."
  handleVmAction fmt "start" vmRef (vmStart conn vmRef wait)

-- | Handle VM stop command. With --wait, the server blocks until VmStopped.
handleVmStop :: OutputFormat -> Connection -> Text -> WaitOptions -> IO Bool
handleVmStop fmt conn vmRef waitOpts = do
  let wait = woWait waitOpts
  unless (isStructured fmt) $
    when wait $
      putStrLn $
        "Stopping VM '" ++ T.unpack vmRef ++ "' and waiting for it to stop..."
  handleVmAction fmt "stop" vmRef (vmStop conn vmRef wait)

-- | Poll the daemon until a VM reaches a target status.
-- Returns True if the target status was reached, False on timeout or error.
waitForVmStatus :: OutputFormat -> Connection -> Text -> VmStatus -> Int -> IO Bool
waitForVmStatus fmt conn vmRef targetStatus timeout = do
  startTime <- getCurrentTime
  go startTime
  where
    go startTime = do
      threadDelay 1000000 -- 1 second
      now <- getCurrentTime
      let elapsed = round (diffUTCTime now startTime) :: Int
      if elapsed >= timeout
        then do
          let msg = "VM '" ++ T.unpack vmRef ++ "' did not reach " ++ T.unpack (enumToText targetStatus) ++ " within " ++ show timeout ++ " seconds."
          emitError fmt "timeout" (T.pack msg) $
            putStrLn $
              "Timeout: " ++ msg
          pure False
        else do
          resp <- showVm conn vmRef
          case resp of
            Right (Just details)
              | vdStatus details == targetStatus -> pure True
              | vdStatus details == VmError -> do
                  let msg = "VM '" ++ T.unpack vmRef ++ "' entered error state."
                  emitError fmt "vm_error" (T.pack msg) $ putStrLn msg
                  pure False
            _ -> do
              unless (isStructured fmt) $ do
                hPutStr stderr "."
                hFlush stderr
              go startTime

-- | Handle VM edit
handleVmEdit :: OutputFormat -> Connection -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO Bool
handleVmEdit fmt conn vmRef mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart = do
  resp <- vmEdit conn vmRef mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right VmEdited -> do
      emitOk fmt $ putStrLn $ "VM '" ++ T.unpack vmRef ++ "' updated."
      pure True
    Right VmEditNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "Error: VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right VmEditMustBeStopped -> do
      emitError fmt "vm_must_be_stopped" ("VM '" <> vmRef <> "' must be stopped to edit") $
        putStrLn $
          "Error: VM '" ++ T.unpack vmRef ++ "' must be stopped to edit properties."
      pure False
    Right (VmEditError msg) -> do
      emitError fmt "edit_failed" msg $
        putStrLn $
          "Failed to edit VM: " ++ T.unpack msg
      pure False

-- | Print VM info in table format
printVmInfo :: UTCTime -> VmInfo -> IO ()
printVmInfo now vm =
  putStrLn $
    printf
      "%-6d %-20s %-12s %5d %8d  %-6s %-2s %-2s"
      (viId vm)
      (T.unpack $ viName vm)
      (T.unpack $ enumToText $ viStatus vm)
      (viCpuCount vm)
      (viRamMb vm)
      (healthLabel now vm)
      (if viCloudInit vm then "+" else "-" :: String)
      (if viAutostart vm then "+" else "-" :: String)

-- | Print full VM details
printVmDetails :: VmDetails -> IO ()
printVmDetails vm = do
  printField "VM ID" (show (vdId vm))
  printField "Name" (T.unpack (vdName vm))
  printField "Created" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (vdCreatedAt vm))
  printField "Status" (T.unpack (enumToText $ vdStatus vm))
  printField "CPUs" (show (vdCpuCount vm))
  printField "RAM (MB)" (show (vdRamMb vm))
  printField "Description" (maybe "(none)" T.unpack (vdDescription vm))
  printField "Console" (if vdHeadless vm then "serial (headless)" else "SPICE (graphics)")
  printField "Guest Agent" (if vdGuestAgent vm then "enabled" else "disabled")
  printField "Cloud-init" (if vdCloudInit vm then "enabled" else "disabled")
  printField "Autostart" (if vdAutostart vm then "enabled" else "disabled")
  case vdCloudInitConfig vm of
    Just _ -> printField "Cloud-init Config" "custom"
    Nothing -> pure ()
  printField "Healthcheck" (maybe "(no data)" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (vdHealthcheck vm))
  printField "Monitor" (T.unpack (vdMonitorSocket vm))
  if vdHeadless vm
    then printField "Serial" (T.unpack (vdSerialSocket vm))
    else printField "SPICE" (T.unpack (vdSpiceSocket vm))
  printField "Guest Agent" (T.unpack (vdGuestAgentSocket vm))

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
      putStrLn $ "    Disk Image: " ++ T.unpack (diDiskImageName d)
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
      case niGuestIpAddresses n of
        Nothing -> pure ()
        Just ips -> putStrLn $ "    Guest IPs: " ++ T.unpack ips

-- | Compute health label for VM list display
healthLabel :: UTCTime -> VmInfo -> String
healthLabel now vm
  | not (viGuestAgent vm) = "--"
  | viStatus vm /= VmRunning = "--"
  | otherwise = case viHealthcheck vm of
      Nothing -> "DOWN"
      Just t -> if diffUTCTime now t < 30 then "OK" else "DOWN"

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

-- | Connect to a Unix socket and run a raw terminal session.
-- Convenience wrapper around 'runRawTerminalSession'.
runMonitorSession :: FilePath -> Maybe FilePath -> IO Bool
runMonitorSession sockPath mMonitorSock = do
  savedAttrs <- getTerminalAttributes stdInput
  result <-
    try $
      bracket
        ( do
            sock <- socket AF_UNIX Stream defaultProtocol
            NS.connect sock (SockAddrUnix sockPath)
            pure sock
        )
        close
        (\sock -> doRawTerminalSession savedAttrs sock mMonitorSock Nothing)
  setTerminalAttributes stdInput savedAttrs Immediately
  hSetBinaryMode stdin False
  hSetBinaryMode stdout False
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "\nFailed to connect: " ++ show e
      pure False
    Right () -> do
      putStrLn "\nDisconnected."
      pure True

-- | Run a raw terminal session on an already-connected socket.
-- Uses true raw terminal mode (cfmakeraw) so that serial consoles,
-- including UEFI firmware menus, work correctly.
--
-- Ctrl+] is the escape prefix:
--   Ctrl+] q   — quit
--   Ctrl+] d   — send Ctrl+Alt+Del (requires monitor socket)
--   Ctrl+] f   — flush serial console buffer
--   Ctrl+] ?   — show help
--   Ctrl+] Ctrl+] — send literal Ctrl+] to the VM
runRawTerminalSession :: Socket -> Maybe FilePath -> Maybe (IO ()) -> IO Bool
runRawTerminalSession sock mMonitorSock mFlushAction = do
  savedAttrs <- getTerminalAttributes stdInput
  result <- try $ doRawTerminalSession savedAttrs sock mMonitorSock mFlushAction
  setTerminalAttributes stdInput savedAttrs Immediately
  hSetBinaryMode stdin False
  hSetBinaryMode stdout False
  case result of
    Left (_ :: SomeException) -> pure False
    Right () -> pure True

--------------------------------------------------------------------------------
-- Raw Terminal Internals
--------------------------------------------------------------------------------

-- | cfmakeraw equivalent: disable all input/output processing
makeRaw :: TerminalAttributes -> TerminalAttributes
makeRaw attrs =
  withMinInput
    ( withTime
        ( withBits
            ( foldl
                withoutMode
                attrs
                [ -- Input: no CR/NL translation, no parity/break handling
                  MapCRtoLF
                , MapLFtoCR
                , IgnoreCR
                , IgnoreBreak
                , InterruptOnBreak
                , MarkParityErrors
                , StripHighBit
                , StartStopInput
                , -- Output: no NL→CR+NL expansion
                  ProcessOutput
                , -- Local: no echo, no canonical mode, no signals
                  EnableEcho
                , EchoLF
                , ProcessInput
                , KeyboardInterrupts
                , ExtendedFunctions
                , -- Control: no parity
                  EnableParity
                ]
            )
            8 -- CS8: 8 bits per byte
        )
        0 -- VTIME = 0
    )
    1 -- VMIN = 1

-- | Send an HMP command via a temporary connection to the monitor socket.
sendMonitorCommand :: FilePath -> String -> IO ()
sendMonitorCommand monSock cmd =
  bracket
    ( do
        s <- socket AF_UNIX Stream defaultProtocol
        NS.connect s (SockAddrUnix monSock)
        pure s
    )
    close
    $ \s -> do
      -- Drain the HMP greeting/prompt
      _ <- recv s 4096
      sendAll s (BS.pack (map (fromIntegral . ord) (cmd ++ "\n")))
      -- Read the response (discard it)
      _ <- recv s 4096
      pure ()

-- | Print a status line in raw terminal mode (uses CR+LF since OPOST is off).
rawPutStrLn :: String -> IO ()
rawPutStrLn s = do
  hPutStr stderr ("\r\n" ++ s ++ "\r\n")
  hFlush stderr

-- | Core raw terminal session logic. Caller must save/restore terminal attributes.
doRawTerminalSession :: TerminalAttributes -> Socket -> Maybe FilePath -> Maybe (IO ()) -> IO ()
doRawTerminalSession savedAttrs sock mMonitorSock mFlushAction = do
  setTerminalAttributes stdInput (makeRaw savedAttrs) Immediately
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  exitVar <- newEmptyMVar

  -- Thread to read from socket and write raw bytes to stdout
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

  -- Read raw bytes from stdin and send to socket.
  -- In raw mode Ctrl+C arrives as byte 0x03 (forwarded to VM),
  -- Enter arrives as CR 0x0D (not translated to NL).
  let inputLoop = do
        c <- getChar
        if ord c == 29 -- Ctrl+]  — escape prefix
          then handleEscape
          else do
            sendAll sock (BS.singleton (fromIntegral $ ord c))
            inputLoop

      handleEscape = do
        e <- getChar
        case e of
          _ | ord e == 29 -> do
            -- Ctrl+] Ctrl+] — send literal Ctrl+] to VM
            sendAll sock (BS.singleton 29)
            inputLoop
          'q' -> putMVar exitVar ()
          'd' -> do
            case mMonitorSock of
              Just monSock -> do
                result' <- try $ sendMonitorCommand monSock "sendkey ctrl-alt-delete"
                case result' of
                  Left (ex :: SomeException) ->
                    rawPutStrLn $ "Failed to send Ctrl+Alt+Del: " ++ show ex
                  Right () ->
                    rawPutStrLn "Sent Ctrl+Alt+Del."
              Nothing ->
                rawPutStrLn "Ctrl+Alt+Del not available (no monitor socket)."
            inputLoop
          'f' -> do
            case mFlushAction of
              Just flush -> do
                result' <- try flush
                case result' of
                  Left (ex :: SomeException) ->
                    rawPutStrLn $ "Failed to flush buffer: " ++ show ex
                  Right () ->
                    rawPutStrLn "Buffer flushed."
              Nothing ->
                rawPutStrLn "Flush not available."
            inputLoop
          '?' -> do
            rawPutStrLn "Escape commands (Ctrl+] prefix):"
            rawPutStrLn "  q         — quit"
            rawPutStrLn "  d         — send Ctrl+Alt+Del"
            rawPutStrLn "  f         — flush serial console buffer"
            rawPutStrLn "  Ctrl+]    — send literal Ctrl+]"
            rawPutStrLn "  ?         — this help"
            inputLoop
          _ -> inputLoop -- unknown escape, ignore

  -- Run input loop, catching exceptions
  _ <- forkIO $ do
    result' <- try inputLoop
    case result' of
      Left (_ :: SomeException) -> putMVar exitVar ()
      Right () -> pure ()

  -- Wait for exit signal
  takeMVar exitVar
