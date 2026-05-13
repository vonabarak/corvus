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

    -- * Polling
  , waitForVmStatus

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

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (Align (..), Column (..), emitError, emitOk, emitOkWith, isStructured, printField)
import Corvus.Client.Types (OutputFormat (..), WaitOptions (..))
import Corvus.Model (EnumText (..), VmStatus (..))
import Corvus.Protocol (DriveInfo (..), NetIfInfo (..), VmDetails (..), VmInfo (..))
import Corvus.Wire.Common (entityRefFromText)
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Network.Socket (Socket)
import System.IO (hFlush, hPutStr, stderr)

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
handleVmCreate :: OutputFormat -> CapnpConnection -> Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> Bool -> Bool -> IO Bool
handleVmCreate fmt conn name cpuCount ramMb mDesc headless guestAgent cloudInit autostart = do
  r <- try @SomeException (CR.rpcVmCreate conn name cpuCount ramMb mDesc headless guestAgent cloudInit autostart)
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

-- | Poll the daemon until a VM reaches a target status.
-- Returns True if the target status was reached, False on timeout or error.
waitForVmStatus :: OutputFormat -> CapnpConnection -> Text -> VmStatus -> Int -> IO Bool
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
          resp <- try (CR.rpcVmShow conn (entityRefFromText vmRef)) :: IO (Either SomeException VmDetails)
          case resp of
            Right details
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
handleVmEdit :: OutputFormat -> CapnpConnection -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO Bool
handleVmEdit fmt conn vmRef mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart =
  tryRpcUnit
    fmt
    (putStrLn $ "VM '" ++ T.unpack vmRef ++ "' updated.")
    (CR.rpcVmEdit conn (entityRefFromText vmRef) mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart)

-- ---------------------------------------------------------------------
-- Streaming stubs (Phase 6 will wire Cap'n Proto sinks)
-- ---------------------------------------------------------------------

-- | Which kind of raw relay session is running. Retained as a
-- placeholder so call sites in 'Corvus.Client.Commands' still
-- type-check; the actual relay is unimplemented in Phase 5.
data RawSessionKind = SerialSession | MonitorSession
  deriving (Eq, Show)

-- | Stubbed serial / HMP terminal session. Returns False so the
-- caller exits non-zero. Phase 6 wires this over a ByteSink cap.
runRawTerminalSession :: Socket -> RawSessionKind -> Maybe (IO ()) -> Maybe (IO ()) -> IO Bool
runRawTerminalSession _ _ _ _ = do
  putStrLn "error: serial console / HMP monitor require Cap'n Proto streaming (Phase 6, not yet implemented)"
  pure False

-- | Stubbed remote-viewer launcher.
--
-- 'CR.rpcVmViewGrant' is wired; this helper was the launcher that
-- materialised the .vv file and ran @remote-viewer@. With the
-- legacy ConnectionError-shaped grant type retired, the caller
-- (Corvus.Client.Commands) now hits CR.rpcVmViewGrant directly. We
-- keep an export here for backwards-compatible call sites.
runRemoteViewer :: a -> b -> IO Bool
runRemoteViewer _ _ = do
  putStrLn "error: SPICE remote-viewer launch is not yet wired through Cap'n Proto (Phase 6)"
  pure False

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
