{-# LANGUAGE OverloadedStrings #-}

-- | Attach-to-a-running-VM handlers: serial console, HMP monitor,
-- Ctrl+Alt+Del injection, and SPICE viewer grants.
--
-- These are validation-heavy: each one checks that the VM is in a
-- state that accepts the attach ('isViewable') and then does the
-- user-facing dispatch (agent RPC / QMP). The ring buffers themselves
-- are owned by the agent; the daemon only validates and relays.
module Corvus.Handlers.Vm.Console
  ( -- * Viewability
    viewableStatuses
  , isViewable

    -- * Serial console
  , handleSerialConsole
  , handleSerialConsoleFlush

    -- * HMP monitor
  , handleHmpMonitor
  , handleHmpMonitorFlush

    -- * Ctrl+Alt+Del
  , handleVmSendCtrlAltDel

    -- * SPICE viewer
  , handleVmViewGrant
  , generateSpicePassword
  )
where

import Corvus.Handlers.Vm.Db (getVmWithStatus)
import Corvus.Model
  ( Vm (vmHeadless, vmSpicePort, vmStatus)
  , VmId
  , VmStatus (..)
  )
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol (Response (..))
import Corvus.Qemu (QemuConfig (qcSpiceBindAddress), QmpResult (..), qmpSendCtrlAltDel)
import Corvus.Types (ServerState, ssDbPool, ssQemuConfig)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist (get)
import Database.Persist.Sql (runSqlPool, toSqlKey)
import System.IO (IOMode (ReadMode), withBinaryFile)

-- | VM statuses in which a user may attach to the console, HMP monitor,
-- or SPICE viewer. Anything non-@stopped@ where QEMU is (or should soon
-- be) alive — deliberately excludes 'VmPaused' (no live I/O) and
-- 'VmError' (QEMU has already died).
viewableStatuses :: [VmStatus]
viewableStatuses = [VmRunning, VmStarting, VmStopping]

-- | True when the VM is in a state that accepts console/monitor/view attach.
isViewable :: VmStatus -> Bool
isViewable = (`elem` viewableStatuses)

-- | Validate that the VM can be addressed for serial console
-- attachment (running + headless). The agent owns the ring buffer;
-- this only does the user-facing-message validation.
handleSerialConsole :: ServerState -> Int64 -> IO Response
handleSerialConsole state vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  pure $ case result of
    Nothing -> RespVmNotFound
    Just (vm, status)
      | not (isViewable status) -> RespVmNotRunning
      | not (vmHeadless vm) -> RespVmHeadless
      | otherwise -> RespSerialConsoleOk

-- | Validate the VM for serial-console flush (same predicate as
-- attach). The actual flush is dispatched through the agent.
handleSerialConsoleFlush :: ServerState -> Int64 -> IO Response
handleSerialConsoleFlush state vmId = do
  resp <- handleSerialConsole state vmId
  pure $ case resp of
    RespSerialConsoleOk -> RespSerialConsoleFlushed
    other -> other

-- | Validate that the VM is running for HMP monitor attachment.
-- Headlessness doesn't matter: HMP exists for both headless and
-- graphical VMs.
handleHmpMonitor :: ServerState -> Int64 -> IO Response
handleHmpMonitor state vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  pure $ case result of
    Nothing -> RespVmNotFound
    Just (_, status)
      | not (isViewable status) -> RespVmNotRunning
      | otherwise -> RespHmpMonitorOk

-- | Validate the VM for HMP-monitor flush; actual flush dispatches
-- through the agent.
handleHmpMonitorFlush :: ServerState -> Int64 -> IO Response
handleHmpMonitorFlush state vmId = do
  resp <- handleHmpMonitor state vmId
  pure $ case resp of
    RespHmpMonitorOk -> RespHmpMonitorFlushed
    other -> other

-- | Inject Ctrl+Alt+Del into a running VM via QMP. Delivered through
-- the daemon's QMP client so it works regardless of whether the
-- caller is on the daemon host.
handleVmSendCtrlAltDel :: ServerState -> Int64 -> IO Response
handleVmSendCtrlAltDel state vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (_, status)
      | not (isViewable status) -> pure RespVmNotRunning
      | otherwise -> do
          qmpResult <- qmpSendCtrlAltDel (ssQemuConfig state) vmId
          case qmpResult of
            QmpSuccess -> pure RespOk
            QmpError err -> pure $ RespError $ "QMP send-key failed: " <> err
            QmpConnectionFailed err -> pure $ RespError $ "QMP connection failed: " <> err

-- | Grant a short-lived SPICE connection for a running non-headless VM.
--
-- Generates a fresh 18-byte (24-char URL-safe base64) random password,
-- installs it via QMP @set_password@, and schedules expiry via
-- @expire_password@ so an unused grant disappears on its own. The
-- daemon never persists the password — it lives in QEMU's in-memory
-- SPICE state until it expires or is rotated by the next grant.
handleVmViewGrant :: ServerState -> Int64 -> IO Response
handleVmViewGrant state vmId = do
  let pool = ssDbPool state
      cfg = ssQemuConfig state
  mVm <- runSqlPool (get (toSqlKey vmId :: VmId)) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm
      | vmHeadless vm -> pure RespVmHeadless
      | not (isViewable (vmStatus vm)) -> pure RespVmNotRunning
      | otherwise -> case vmSpicePort vm of
          Nothing -> pure $ RespError "VM has no SPICE port assigned (daemon bug)"
          Just spicePort -> do
            pw <- generateSpicePassword
            let ttl = 120 :: Int
            outer <- withVmNodeAgent state vmId $ \nac ->
              NOA.vmSetSpiceTicket nac vmId pw (fromIntegral ttl)
            case outer of
              Left err -> pure $ RespError err
              Right r -> case r of
                Left e ->
                  pure $ RespError $ "vmSetSpiceTicket: " <> T.pack (show e)
                Right () ->
                  pure $
                    RespVmViewGrant
                      { host = qcSpiceBindAddress cfg
                      , port = spicePort
                      , password = pw
                      , ttlSeconds = ttl
                      }

-- | Read 18 bytes from @/dev/urandom@ and encode as URL-safe base64
-- (24 printable characters, no padding issues in SPICE tickets).
generateSpicePassword :: IO Text
generateSpicePassword = do
  bytes <- withBinaryFile "/dev/urandom" ReadMode $ \h -> BS.hGet h 18
  pure $ TE.decodeUtf8 $ B64URL.encode bytes
