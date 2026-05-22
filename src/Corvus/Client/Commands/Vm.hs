{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | VM command handlers for the Corvus client.
--
-- Rewritten in Phase 5 to talk Cap'n Proto via
-- 'Corvus.Client.Capnp.Rpc'. The old fine-grained result sums
-- ('VmActionResult', 'VmCreateResult', ...) are gone; each handler
-- now uses 'try @SomeException' around the cap call and renders
-- success / failure uniformly.
--
-- The serial console / HMP monitor relay (which used to take a raw
-- socket and ride a protocol upgrade) is stubbed: those flows need
-- Cap'n Proto streaming sinks and land in Phase 6.
module Corvus.Client.Commands.Vm
  ( -- * VM command handlers
    handleVmCreate
  , handleVmDelete
  , handleVmAction
  , handleVmStart
  , handleVmStop
  , handleVmEdit
  , handleVmMigrate

    -- * VM display/interaction
  , runRemoteViewer
  , runRawTerminalSession
  , RawSessionKind (..)

    -- * Formatters
  , vmColumns
  , printVmDetails
  , formatUptime
  )
where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, finally, try)
import Control.Monad (unless, when)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Config (ClientConfig (..))
import Corvus.Client.Output (Align (..), Column (..), emitError, emitOk, emitOkWith, isStructured, printField)
import Corvus.Client.Types (OutputFormat (..), WaitOptions (..))
import Corvus.Model (EnumText (..), VmStatus (..))
import Corvus.Protocol (DriveInfo (..), NetIfInfo (..), VmDetails (..), VmInfo (..))
import Corvus.Wire.Common (ViewGrant (..), entityRefFromText)
import Data.Aeson (toJSON)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import System.IO (BufferMode (..), hClose, hFlush, hPutStr, hSetBinaryMode, hSetBuffering, stderr, stdin, stdout)
import System.IO.Temp (withSystemTempFile)
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
import System.Posix.IO (fdWrite, stdInput, stdOutput)
import System.Posix.Terminal
import System.Process (callProcess)

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

-- | Run a Cap'n Proto call that returns unit (or a value we discard).
-- Render @ok@ on success or a typed @rpc_error@ on failure.
tryRpcUnit :: OutputFormat -> IO () -> IO () -> IO Bool
tryRpcUnit fmt successText action = do
  r <- try action :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt successText
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) (putStrLn ("Error: " <> show e))
      pure False

-- ---------------------------------------------------------------------
-- VM creation / deletion
-- ---------------------------------------------------------------------

-- | Handle VM creation
handleVmCreate
  :: OutputFormat
  -> CapnpConnection
  -> Text
  -- ^ name
  -> Text
  -- ^ node ref (name or id)
  -> Int
  -> Int
  -> Maybe Text
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -- ^ rebootQuirk
  -> IO Bool
handleVmCreate fmt conn name nodeRef cpuCount ramMb mDesc headless guestAgent cloudInit autostart rebootQuirk = do
  r <- try @SomeException (CR.rpcVmCreate conn name nodeRef cpuCount ramMb mDesc headless guestAgent cloudInit autostart rebootQuirk)
  case r of
    Right vmId -> do
      emitOkWith fmt [("id", toJSON vmId)] $
        putStrLn $
          "VM '" ++ T.unpack name ++ "' created with ID: " ++ show vmId
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn $
          "Failed to create VM: " ++ show e
      pure False

-- | Handle VM deletion
handleVmDelete :: OutputFormat -> CapnpConnection -> Text -> Bool -> IO Bool
handleVmDelete fmt conn vmRef deleteDisks =
  tryRpcUnit
    fmt
    (putStrLn $ "VM '" ++ T.unpack vmRef ++ "' deleted.")
    (CR.rpcVmDelete conn (entityRefFromText vmRef) deleteDisks)

-- | Generic VM action shim. Kept for compatibility with the
-- dispatcher in 'Corvus.Client.Commands' which threads an
-- 'actionName' through for messages.
handleVmAction :: OutputFormat -> String -> Text -> IO () -> IO Bool
handleVmAction fmt actionName vmRef action = do
  r <- try action :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn $ "VM '" ++ T.unpack vmRef ++ "' " ++ actionName ++ ": OK"
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn $
          "Cannot " ++ actionName ++ " VM '" ++ T.unpack vmRef ++ "': " ++ show e
      pure False

-- | Handle VM start. With @--wait@, blocks on the cap call.
handleVmStart :: OutputFormat -> CapnpConnection -> Text -> WaitOptions -> IO Bool
handleVmStart fmt conn vmRef waitOpts = do
  let wait = woWait waitOpts
  unless (isStructured fmt) $
    when wait $
      putStrLn $
        "Starting VM '" ++ T.unpack vmRef ++ "' and waiting for it to become running..."
  handleVmAction fmt "start" vmRef (CR.rpcVmStart conn (entityRefFromText vmRef) wait)

-- | Handle VM stop. With @--wait@, blocks on the cap call.
handleVmStop :: OutputFormat -> CapnpConnection -> Text -> WaitOptions -> IO Bool
handleVmStop fmt conn vmRef waitOpts = do
  let wait = woWait waitOpts
  unless (isStructured fmt) $
    when wait $
      putStrLn $
        "Stopping VM '" ++ T.unpack vmRef ++ "' and waiting for it to stop..."
  handleVmAction fmt "stop" vmRef (CR.rpcVmStop conn (entityRefFromText vmRef) wait)

-- | Handle VM edit
handleVmEdit
  :: OutputFormat
  -> CapnpConnection
  -> Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -- ^ rebootQuirk
  -> IO Bool
handleVmEdit fmt conn vmRef mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart mRebootQuirk =
  tryRpcUnit
    fmt
    (putStrLn $ "VM '" ++ T.unpack vmRef ++ "' updated.")
    (CR.rpcVmEdit conn (entityRefFromText vmRef) mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart mRebootQuirk)

-- | Handle @crv vm migrate <VM> --to-node <NODE>@.
handleVmMigrate :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleVmMigrate fmt conn vmRef toNodeRef = do
  r <-
    try @SomeException $
      CR.rpcVmMigrate
        conn
        (entityRefFromText vmRef)
        (entityRefFromText toNodeRef)
  case r of
    Right tid -> do
      emitOkWith fmt [("taskId", toJSON tid)] $
        putStrLn $
          "VM migration started. Task ID: " ++ show tid
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error migrating VM: " ++ show e)
      pure False

-- ---------------------------------------------------------------------
-- Raw terminal session over Cap'n Proto ByteSink caps (Phase 6b/c/f)
-- ---------------------------------------------------------------------

-- | Which kind of raw relay session is running. Controls the
-- escape-help text, the wording of the flush confirmation, and
-- whether @Ctrl+] d@ (send Ctrl+Alt+Del via HMP) is offered at
-- all. HMP monitor sessions hide the key entirely because "send
-- Ctrl+Alt+Del" has no meaning at the monitor layer.
data RawSessionKind = SerialSession | MonitorSession
  deriving (Eq, Show)

-- | Run an interactive raw-terminal session against a pair of
-- 'writeInput' / 'endInput' closures returned by
-- 'CR.rpcVmSerialConsole' or 'CR.rpcVmHmpMonitor'. Output from
-- the daemon arrives on a separate path (the caller wires the
-- sink's @onChunk@ callback directly to 'stdout'); this function
-- handles only the input side and the user's escape sequences.
--
-- Returns when either:
--
--   * The user types @Ctrl+] q@ (escape prefix then @q@), or
--   * the caller puts @()@ on @endSignal@ (typically from the
--     output sink's @onEnd@ callback when the daemon closes the
--     relay).
--
-- Termios manipulation mirrors @cfmakeraw(3)@ so UEFI menus and
-- bracketed-paste applications work; the terminal is restored on
-- every exit path.
runRawTerminalSession
  :: (BS.ByteString -> IO ())
  -- ^ Forward a chunk of stdin to the daemon's input sink.
  -> IO ()
  -- ^ Close the daemon's input sink. Invoked once before the
  -- terminal is restored.
  -> MVar ()
  -- ^ External exit signal; the caller @putMVar@s once if the
  -- daemon closes its output sink (VM exit, agent disconnect).
  -> RawSessionKind
  -> Maybe (IO ())
  -- ^ Optional @Ctrl+] d@ action (send Ctrl+Alt+Del). Only
  -- offered for 'SerialSession'; @Nothing@ for HMP.
  -> Maybe (IO ())
  -- ^ Optional @Ctrl+] f@ action (flush ring buffer).
  -> IO Bool
runRawTerminalSession writeInput endInput endSignal kind mCtrlAltDelAction mFlushAction = do
  savedAttrs <- getTerminalAttributes stdInput
  let restore = do
        hSetBinaryMode stdin False
        hSetBinaryMode stdout False
        hSetBuffering stdin LineBuffering
        hSetBuffering stdout LineBuffering
        setTerminalAttributes stdInput savedAttrs Immediately
        _ <- fdWrite stdOutput "\r"
        pure ()
  let cleanup = do
        _ <- try endInput :: IO (Either SomeException ())
        restore
  result <-
    try $
      doRawTerminalSession
        savedAttrs
        writeInput
        endSignal
        kind
        mCtrlAltDelAction
        mFlushAction
        `finally` cleanup
  case result of
    Left (_ :: SomeException) -> pure False
    Right () -> pure True

-- | Launch @remote-viewer@ against a SPICE TCP endpoint.
--
-- The short-lived password from 'CR.rpcVmViewGrant' is delivered via
-- a VirtViewer @.vv@ connection file (@[virt-viewer]@ INI section),
-- passed as @remote-viewer@'s sole argument. We deliberately avoid
-- @spice://USER:PASS\@HOST:PORT@-style URIs: that puts the password
-- in argv, visible to any local user via @ps(1)@ or shell history.
-- The temp file is created @0600@ inside the system tempdir and is
-- removed by 'withSystemTempFile' as soon as @remote-viewer@ exits.
runRemoteViewer :: ClientConfig -> ViewGrant -> IO Bool
runRemoteViewer config grant = do
  let viewer = ccRemoteViewer config
      body =
        T.unlines
          [ "[virt-viewer]"
          , "type=spice"
          , "host=" <> vgHost grant
          , "port=" <> T.pack (show (vgPort grant))
          , "password=" <> vgPassword grant
          ]
  result <- try $ withSystemTempFile "corvus-spice-.vv" $ \path handle -> do
    hClose handle
    setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)
    TIO.writeFile path body
    callProcess viewer [path]
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "Failed to run remote-viewer: " ++ show e
      pure False
    Right () -> pure True

--------------------------------------------------------------------------------
-- Raw Terminal Internals
--------------------------------------------------------------------------------

-- | cfmakeraw equivalent: disable all input/output processing.
makeRaw :: TerminalAttributes -> TerminalAttributes
makeRaw attrs =
  withMinInput
    ( withTime
        ( withBits
            ( foldl
                withoutMode
                attrs
                [ MapCRtoLF
                , MapLFtoCR
                , IgnoreCR
                , IgnoreBreak
                , InterruptOnBreak
                , MarkParityErrors
                , StripHighBit
                , StartStopInput
                , ProcessOutput
                , EnableEcho
                , EchoLF
                , ProcessInput
                , KeyboardInterrupts
                , ExtendedFunctions
                , EnableParity
                ]
            )
            8 -- CS8
        )
        0 -- VTIME = 0
    )
    1 -- VMIN = 1

-- | Print a status line in raw terminal mode (uses CR+LF since OPOST is off).
rawPutStrLn :: String -> IO ()
rawPutStrLn s = do
  hPutStr stderr ("\r\n" ++ s ++ "\r\n")
  hFlush stderr

doRawTerminalSession
  :: TerminalAttributes
  -> (BS.ByteString -> IO ())
  -> MVar ()
  -> RawSessionKind
  -> Maybe (IO ())
  -> Maybe (IO ())
  -> IO ()
doRawTerminalSession savedAttrs writeInput endSignal kind mCtrlAltDelAction mFlushAction = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  setTerminalAttributes stdInput (makeRaw savedAttrs) Immediately

  exitVar <- newEmptyMVar

  -- Mirror the external 'endSignal' onto the local exit MVar.
  endWatcher <- forkIO $ do
    takeMVar endSignal
    _ <- tryPutMVar exitVar ()
    pure ()

  let inputLoop = do
        c <- getChar
        if ord c == 29 -- Ctrl+]
          then handleEscape
          else do
            writeInput (BS.singleton (fromIntegral (ord c)))
            inputLoop

      handleEscape = do
        e <- getChar
        case e of
          _ | ord e == 29 -> do
            writeInput (BS.singleton 29)
            inputLoop
          'q' -> putMVar exitVar ()
          'd' -> do
            case kind of
              MonitorSession -> inputLoop
              SerialSession -> do
                case mCtrlAltDelAction of
                  Just action -> do
                    r' <- try action :: IO (Either SomeException ())
                    case r' of
                      Left ex -> rawPutStrLn ("Failed to send Ctrl+Alt+Del: " <> show ex)
                      Right () -> rawPutStrLn "Sent Ctrl+Alt+Del."
                  Nothing -> rawPutStrLn "Ctrl+Alt+Del not available."
                inputLoop
          'f' -> do
            case mFlushAction of
              Just flush -> do
                r' <- try flush :: IO (Either SomeException ())
                case r' of
                  Left ex -> rawPutStrLn ("Failed to flush buffer: " <> show ex)
                  Right () -> rawPutStrLn "Buffer flushed."
              Nothing -> rawPutStrLn "Flush not available."
            inputLoop
          '?' -> do
            rawPutStrLn "Escape commands (Ctrl+] prefix):"
            rawPutStrLn "  q         — quit"
            case kind of
              SerialSession -> rawPutStrLn "  d         — send Ctrl+Alt+Del"
              MonitorSession -> pure ()
            case kind of
              SerialSession -> rawPutStrLn "  f         — flush serial console buffer"
              MonitorSession -> rawPutStrLn "  f         — flush HMP monitor buffer"
            rawPutStrLn "  Ctrl+]    — send literal Ctrl+]"
            rawPutStrLn "  ?         — this help"
            inputLoop
          _ -> inputLoop

  inputThread <- forkIO $ do
    r' <- try inputLoop :: IO (Either SomeException ())
    case r' of
      Left _ -> putMVar exitVar ()
      Right () -> pure ()

  takeMVar exitVar
  killThread inputThread
  killThread endWatcher

-- ---------------------------------------------------------------------
-- Display
-- ---------------------------------------------------------------------

-- | Column definitions for the @vm list@ table.
vmColumns :: UTCTime -> [Column VmInfo]
vmColumns now =
  [ Column "ID" RightAlign (show . viId)
  , Column "NAME" LeftAlign (T.unpack . viName)
  , Column "STATUS" LeftAlign (T.unpack . enumToText . viStatus)
  , Column "CPUS" RightAlign (show . viCpuCount)
  , Column "RAM_MB" RightAlign (show . viRamMb)
  , Column "HEALTH" LeftAlign (healthLabel now)
  , Column "CI" LeftAlign (\vm -> if viCloudInit vm then "+" else "-")
  , Column "AS" LeftAlign (\vm -> if viAutostart vm then "+" else "-")
  ]

-- | Print full VM details
printVmDetails :: VmDetails -> IO ()
printVmDetails vm = do
  printField "VM ID" (show (vdId vm))
  printField "Name" (T.unpack (vdName vm))
  printField "Created" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (vdCreatedAt vm))
  printField "Status" (T.unpack (enumToText $ vdStatus vm))
  case vdErrorMessage vm of
    Just msg -> do
      -- Print the timestamp on the "Last error" line so it's
      -- always visible; reflow the (possibly multi-line) message
      -- under it with the same 16-col indent printField uses for
      -- value alignment.
      let stamp = case vdLastErrorAt vm of
            Just at -> formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" at
            Nothing -> "(no timestamp)"
          indented =
            T.unpack
              . T.intercalate "\n                "
              . T.lines
              $ msg
      printField "Last error" stamp
      putStrLn $ "                " ++ indented
    Nothing -> pure ()
  printField "CPUs" (show (vdCpuCount vm))
  printField "RAM (MB)" (show (vdRamMb vm))
  printField "Description" (maybe "(none)" T.unpack (vdDescription vm))
  printField "Console" (if vdHeadless vm then "serial (headless)" else "SPICE (graphics)")
  printField "Guest Agent" (if vdGuestAgent vm then "enabled" else "disabled")
  printField "Cloud-init" (if vdCloudInit vm then "enabled" else "disabled")
  printField "Autostart" (if vdAutostart vm then "enabled" else "disabled")
  printField "Reboot quirk" (if vdRebootQuirk vm then "enabled" else "disabled")
  case vdCloudInitConfig vm of
    Just _ -> printField "Cloud-init Config" "custom"
    Nothing -> pure ()
  printField "Healthcheck" (maybe "(no data)" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (vdHealthcheck vm))
  printField "Monitor" (T.unpack (vdMonitorSocket vm))
  if vdHeadless vm
    then printField "Serial" (T.unpack (vdSerialSocket vm))
    else printField "SPICE port" (maybe "(not running)" show (vdSpicePort vm))
  case vdVsockCid vm of
    Nothing -> pure ()
    Just cid -> do
      printField "Vsock CID" (show cid)
      printField "SSH (vsock)" $ "ssh <user>@vsock%" ++ show cid
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
