{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Utility functions extracted from "Corvus.Node.Caps.Session".
--
-- This module contains decoders, encoders, and infrastructure helpers
-- that are shared across the session cap handler implementations.
module Corvus.Node.Caps.Session.Utils
  ( -- * Disk encoders / parsers
    parseFormat
  , encodeDiskOpResult
  , decodeQuiesceMode
  , encodeDiskInspectInfo
  , encodeDiskSnapshotInfo

    -- * Chardev streaming constants
  , serialBufferCapacity
  , monitorBufferCapacity

    -- * QMP helpers
  , retryBlockdevDel
  , isBlockdevBusy
  , requireRemovableDrive

    -- * QEMU config
  , agentQemuConfig
  , vfsBinary

    -- * VM spawn helpers
  , waitForFirstQgaPing
  , forwardPipeToLog
  , captureStderrTail
  , stderrTailCapacity

    -- * VM lifecycle helpers
  , pollForExit

    -- * Tiny helpers
  , tshow

    -- * Chardev helpers
  , flushBufferForVm

    -- * SessionCap type
  , SessionCap (..)
  , newSessionCap
  , withVmOpLock
  , vmOpLockFor
  )
where

import qualified Capnp.Gen.Enums as CGE
import qualified Capnp.Gen.Nodeagent as CGNA
import Capnp.Rpc (throwFailed)
import Control.Concurrent (MVar, newMVar, threadDelay, withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import qualified Control.Exception as E
import Control.Monad.Logger (logDebugN, runStderrLoggingT)
import qualified Corvus.Model as M
import qualified Corvus.Node.CloudInit as NCI
import qualified Corvus.Node.GuestAgent as NGA
import qualified Corvus.Node.Image as NI
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.Qmp as NQ
import qualified Corvus.Node.SnapshotLive as NSL
import Corvus.Node.SocketBuffer (flushBuffer)
import qualified Corvus.Node.StatusPoller as SP
import qualified Corvus.Node.Transfer as NTr
import qualified Corvus.Node.VmSpec as VS
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import qualified Corvus.Tls as Tls
import Corvus.Types (SocketBufferHandle (..))
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Supervisors (Supervisor)
import System.IO (BufferMode (..), Handle, hClose, hGetLine, hIsEOF, hSetBuffering)

-- ---------------------------------------------------------------------------
-- Disk encoders / parsers

-- | Decode the wire-level format string into a daemon-side
-- 'DriveFormat'. The wire uses the same lowercase tokens
-- ("qcow2", "raw", …) the daemon serialises elsewhere.
parseFormat :: Text -> IO M.DriveFormat
parseFormat t = case M.enumFromText t of
  Right f -> pure f
  Left _ -> throwFailed ("unknown disk format: " <> t)

encodeDiskOpResult :: NI.ImageResult -> CGNA.Parsed CGNA.DiskOpResult
encodeDiskOpResult r = case r of
  NI.ImageSuccess ->
    CGNA.DiskOpResult {CGNA.kind = CGNA.DiskOpKind'success, CGNA.message = ""}
  NI.ImageError msg ->
    CGNA.DiskOpResult {CGNA.kind = CGNA.DiskOpKind'errorGeneric, CGNA.message = msg}
  NI.ImageNotFound ->
    CGNA.DiskOpResult {CGNA.kind = CGNA.DiskOpKind'errorNotFound, CGNA.message = ""}
  NI.ImageFormatNotSupported msg ->
    CGNA.DiskOpResult
      { CGNA.kind = CGNA.DiskOpKind'errorFormatUnsupported
      , CGNA.message = msg
      }

-- | Translate the wire 'QuiesceMode' to the local NSL.QuiesceMode.
-- Unknown future variants are conservative — they fall back to
-- 'NSL.QuiesceAuto', the default mode.
decodeQuiesceMode :: CGE.QuiesceMode -> NSL.QuiesceMode
decodeQuiesceMode CGE.QuiesceMode'auto = NSL.QuiesceAuto
decodeQuiesceMode CGE.QuiesceMode'require = NSL.QuiesceRequire
decodeQuiesceMode CGE.QuiesceMode'skip = NSL.QuiesceSkip
decodeQuiesceMode (CGE.QuiesceMode'unknown' _) = NSL.QuiesceAuto

encodeDiskInspectInfo :: NI.ImageInfo -> CGNA.Parsed CGNA.DiskInspectInfo
encodeDiskInspectInfo info =
  CGNA.DiskInspectInfo
    { CGNA.format = M.enumToText (NI.iiFormat info)
    , CGNA.virtualSizeMb = NI.iiVirtualSizeMb info
    , CGNA.actualSizeMb = fromMaybe 0 (NI.iiActualSizeMb info)
    , CGNA.hasActualSize = isJust (NI.iiActualSizeMb info)
    , CGNA.snapshots = map encodeDiskSnapshotInfo (NI.iiSnapshots info)
    }

encodeDiskSnapshotInfo :: NI.SnapshotData -> CGNA.Parsed CGNA.DiskSnapshotInfo
encodeDiskSnapshotInfo s =
  CGNA.DiskSnapshotInfo
    { CGNA.id = NI.sdId s
    , CGNA.name = NI.sdName s
    , CGNA.sizeMb = maybe 0 fromIntegral (NI.sdSizeMb s) :: Int64
    , CGNA.hasSize = isJust (NI.sdSizeMb s)
    }

-- ---------------------------------------------------------------------------
-- Chardev streaming constants

-- | Ring-buffer capacity for a VM's serial console (1 MiB).
serialBufferCapacity :: Int
serialBufferCapacity = 1048576

-- | Ring-buffer capacity for a VM's HMP monitor scrollback (64 KiB).
monitorBufferCapacity :: Int
monitorBufferCapacity = 65536

-- ---------------------------------------------------------------------------
-- QMP helpers

-- | QEMU @device_del@ completes asynchronously; an immediate
-- @blockdev-del@ on the same node trips a "node N is busy"
-- error. Retry for up to ten seconds with 100 ms backoff; this also
-- leaves time for the guest to acknowledge PCI hot-unplug. Mirrors the
-- pre-Phase-4 daemon-side retry loop in
-- @Corvus.Handlers.Disk.Attach.qmpBlockdevDelRetry@.
retryBlockdevDel
  :: QemuConfig -> Int64 -> Text -> Int -> IO NQ.QmpResult
retryBlockdevDel cfg vid nodeName = go
  where
    go 0 = NQ.qmpBlockdevDel cfg vid nodeName
    go n = do
      r <- NQ.qmpBlockdevDel cfg vid nodeName
      case r of
        NQ.QmpError err | isBlockdevBusy err -> do
          threadDelay 100000
          go (n - 1)
        _ -> pure r

isBlockdevBusy :: Text -> Bool
isBlockdevBusy err =
  "is in use" `T.isInfixOf` err || "is busy" `T.isInfixOf` err

-- | Verify via @query-block@ that the legacy @-drive id=...@ backend
-- for 'driveId' exists and is @removable@, returning the backend
-- name (@"drive-<driveId>"@) on success. Run before @eject@ /
-- @blockdev-change-medium@ so a non-removable or unknown drive is
-- rejected with a descriptive error instead of a raw QMP failure.
requireRemovableDrive :: Int64 -> Int64 -> IO Text
requireRemovableDrive vid driveId = do
  let nodeName = "drive-" <> T.pack (show driveId)
  qres <- NQ.qmpQueryBlock agentQemuConfig vid
  case qres of
    Left err -> throwFailed ("query-block: " <> err)
    Right entries ->
      case find (\be -> NQ.beDevice be == Just nodeName) entries of
        Just be
          | NQ.beRemovable be -> pure nodeName
          | otherwise ->
              throwFailed $
                "drive "
                  <> T.pack (show driveId)
                  <> " does not support media eject/change (not removable)"
        Nothing ->
          throwFailed $
            "drive "
              <> T.pack (show driveId)
              <> " has no QEMU block backend (drive unknown to QEMU)"

-- ---------------------------------------------------------------------------
-- QEMU config

-- | Local QEMU/runtime config used by every VM-abstraction handler.
--
-- A copy of 'Corvus.Qemu.Config.defaultQemuConfig'. The daemon's
-- choice of @qcBasePath@ / @qcRuntimeDir@ defaults to the same
-- XDG-derived layout, so a fresh @QemuConfig@ inside the agent
-- produces the same paths.
agentQemuConfig :: QemuConfig
agentQemuConfig = defaultQemuConfig

-- ---------------------------------------------------------------------------
-- VM spawn helpers

-- | Poll QGA every 200 ms up to @timeoutMs@ ms; return 'True' as
-- soon as one ping succeeds, 'False' on timeout. Used by
-- 'doVmStart' to block until the guest agent inside the VM is
-- alive.
--
-- The 'NGA.GuestAgentConns' argument MUST be the agent-wide cache
-- ('scQgaConns'), not a fresh local map. The successful ping
-- caches a live QGA socket in the cache's per-VM MVar; if we
-- used a private map, that socket would stay live but
-- unreferenced after this function returned, and the status
-- poller's subsequent connect to the same QGA chardev would
-- collide with it (QEMU's chardev has backlog=1) and fail with
-- @EAGAIN / Resource temporarily unavailable@.
waitForFirstQgaPing
  :: NGA.GuestAgentConns
  -> QemuConfig
  -> Int64
  -> TVar (Maybe Int)
  -- ^ 'vlsLastExitCode' — set by the reaper the moment QEMU
  -- exits, so we can surface an accurate error instead of timing
  -- out.
  -> TVar T.Text
  -- ^ 'vlsStderrTail' — last ~4 KiB of QEMU's stderr, included
  -- verbatim in the error string when QEMU died early.
  -> Word32
  -- ^ wall-clock budget in milliseconds
  -> IO (Either T.Text ())
  -- ^ 'Right ()' when the first guest-agent ping succeeded;
  -- 'Left reason' on timeout *or* early QEMU exit. The reason
  -- string is intended to flow verbatim into the daemon's task
  -- message and 'vm.error_message'.
waitForFirstQgaPing conns cfg vmId exitVar stderrVar timeoutMs = do
  let budgetSec = fromIntegral timeoutMs / 1000 :: Double
  start <- getMonotonicTime
  loop (start + budgetSec)
  where
    loop deadline = do
      mExit <- readTVarIO exitVar
      case mExit of
        Just code -> Left <$> earlyExitReason code
        Nothing -> do
          ok <- NGA.guestPing conns cfg vmId
          if ok
            then pure $ Right ()
            else do
              -- Re-check the reaper after the (potentially slow)
              -- ping attempt: 'guestPing' has its own ~15 s internal
              -- timeout, so QEMU could have died during the call.
              mExit' <- readTVarIO exitVar
              case mExit' of
                Just code -> Left <$> earlyExitReason code
                Nothing -> do
                  now <- getMonotonicTime
                  if now >= deadline
                    then
                      pure $
                        Left $
                          "guest agent did not respond within "
                            <> tshow timeoutMs
                            <> " ms for vmId "
                            <> tshow vmId
                    else threadDelay 200000 >> loop deadline

    earlyExitReason code = do
      stderr <- readTVarIO stderrVar
      let trimmed = T.strip stderr
          tail' = if T.null trimmed then T.empty else "; stderr tail: " <> trimmed
      pure $
        "QEMU for vmId "
          <> tshow vmId
          <> " exited with code "
          <> tshow code
          <> " before first guest-agent ping"
          <> tail'

-- | Drain a child-process pipe line-by-line, forwarding each line
-- to the agent log at debug level under @label@. Used for every
-- subprocess the agent spawns (QEMU stdout, virtiofsd stdout +
-- stderr, …) so the operator can opt into seeing exactly what the
-- helpers print without leaving the pipes unread (which would
-- back-pressure the child once the kernel buffer fills, and leave
-- zombies behind if the child later exited on its own).
forwardPipeToLog :: Text -> Handle -> IO ()
forwardPipeToLog label h = do
  hSetBuffering h LineBuffering
  let go = do
        eof <- hIsEOF h
        if eof
          then pure ()
          else do
            r <- E.try @E.SomeException (hGetLine h)
            case r of
              Left _ -> pure ()
              Right line -> do
                runStderrLoggingT . logDebugN $
                  "[" <> label <> "] " <> T.pack line
                go
  E.handle (\(_ :: E.SomeException) -> pure ()) go
  E.handle (\(_ :: E.SomeException) -> pure ()) (hClose h)

-- | Tail-capture a child-process pipe into a 'TVar' Text, keeping
-- the last 'stderrTailCapacity' bytes. Used to surface QEMU's own
-- diagnostic output when the wait-for-ping path needs to explain
-- why the VM died. Each line is additionally forwarded to the
-- agent log at debug level under @label@ for live visibility.
captureStderrTail :: Text -> Handle -> TVar T.Text -> IO ()
captureStderrTail label h ringVar = do
  hSetBuffering h LineBuffering
  let go = do
        eof <- hIsEOF h
        if eof
          then pure ()
          else do
            r <- E.try @E.SomeException (hGetLine h)
            case r of
              Left _ -> pure ()
              Right line -> do
                runStderrLoggingT . logDebugN $
                  "[" <> label <> "] " <> T.pack line
                let lineT = T.pack line <> "\n"
                atomically $
                  modifyTVar' ringVar $ \prev ->
                    let combined = prev <> lineT
                        overflow = T.length combined - stderrTailCapacity
                     in if overflow > 0
                          then T.drop overflow combined
                          else combined
                go
  E.handle (\(_ :: E.SomeException) -> pure ()) go
  E.handle (\(_ :: E.SomeException) -> pure ()) (hClose h)

-- | Maximum number of characters retained in 'vlsStderrTail'. Sized
-- to fit QEMU's typical "could not …" / KVM-init / device-init
-- error block (a few hundred bytes) with comfortable headroom; not
-- so large that it inflates the daemon's task-message column.
stderrTailCapacity :: Int
stderrTailCapacity = 4096

-- ---------------------------------------------------------------------------
-- VM lifecycle helpers

-- | Poll @vlsLastExitCode@ every 100 ms until non-Nothing or
-- timeout. Returns 'True' when the reaper has filled the var
-- (i.e. QEMU has exited), 'False' on timeout.
pollForExit :: TVar (Maybe Int) -> Int -> IO Bool
pollForExit var timeoutSec = go (max 0 timeoutSec * 10)
  where
    go remaining
      | remaining <= 0 = isJust <$> readTVarIO var
      | otherwise = do
          mExit <- readTVarIO var
          case mExit of
            Just _ -> pure True
            Nothing -> threadDelay 100000 >> go (remaining - 1)

-- ---------------------------------------------------------------------------
-- Tiny helpers

vfsBinary :: QemuConfig -> FilePath
vfsBinary = qcVirtiofsdBinary

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- | Flush the ring buffer for a given vmId. No-op when the vmId
-- has no buffer yet (e.g. the handler was called before vmStart
-- or the buffer was already flushed).
flushBufferForVm
  :: TVar (Map.Map Int64 SocketBufferHandle) -> Int64 -> IO ()
flushBufferForVm bufMapVar vid = do
  bufMap <- readTVarIO bufMapVar
  case Map.lookup vid bufMap of
    Nothing -> pure ()
    Just handle -> flushBuffer (sbhBuffer handle)

-- ---------------------------------------------------------------------------
-- SessionCap type

-- | Session state. Holds:
--
--   * 'scSup' — supervisor used to export server-side caps that
--     handlers create (e.g. the inbound chardev sink returned by
--     @openSerialConsole@).
--   * 'scVmLedger' — vmId-keyed live state for the VM-abstraction
--     methods (vmStart / vmStop* / vmStatus / vmGuestExec / …);
--   * 'scSubs' — registry of 'VmStatusSink' caps the
--     'subscribeVmStatus' handler appends to;
--   * 'scQgaConns' — agent-wide per-VM QGA persistent-socket
--     cache shared by 'vmGuestExec' and the status poller.
--   * 'scSerialBuffers' / 'scMonitorBuffers' — agent-wide
--     per-VM chardev ring-buffer registries the chardev
--     streaming RPCs (@openSerialConsole@, @openHmpMonitor@)
--     read from. Populated during @vmStart@.
data SessionCap = SessionCap
  { scOwner :: !Text
  , scSup :: !Supervisor
  , scVmLedger :: !L.VmLedger
  , scSubs :: !SP.Subscribers
  , scQgaConns :: !NGA.GuestAgentConns
  , scSerialBuffers :: !(TVar (Map.Map Int64 SocketBufferHandle))
  , scMonitorBuffers :: !(TVar (Map.Map Int64 SocketBufferHandle))
  , scTransferTokens :: !NTr.TokenRegistry
  , scTlsConfig :: !(Maybe Tls.TlsConfig)
  , scVmOpLocks :: !(TVar (Map.Map Int64 (MVar ())))
  -- ^ Per-VM lifecycle serialisation. The long lifecycle
  -- handlers ('vmStart' / 'vmStopGraceful' / 'vmSave' /
  -- 'snapshotCreateLive') run via 'handleParsedAsync' so they
  -- don't block the session dispatcher; that drops the implicit
  -- ordering the single 'runServer' loop used to give them, so
  -- this restores per-VM serialisation (two starts, or a stop
  -- racing a start, on the SAME vmId) without blocking other VMs
  -- or read-only calls. 'vmStopHard' deliberately does NOT take
  -- this lock — it must be able to interrupt a stuck graceful
  -- stop. See 'withVmOpLock'.
  , scVsockLaunchLock :: !(MVar ())
  -- ^ Process-wide gate for the host's vhost-vsock CID namespace.
  }

newSessionCap
  :: Text
  -> Supervisor
  -> L.VmLedger
  -> SP.Subscribers
  -> NGA.GuestAgentConns
  -> TVar (Map.Map Int64 SocketBufferHandle)
  -> TVar (Map.Map Int64 SocketBufferHandle)
  -> NTr.TokenRegistry
  -> Maybe Tls.TlsConfig
  -> MVar ()
  -> IO SessionCap
newSessionCap owner sup vmLedger subs qgaConns serialBufs monitorBufs tokens tlsCfg vsockLaunchLock = do
  vmOpLocks <- newTVarIO Map.empty
  pure
    SessionCap
      { scOwner = owner
      , scSup = sup
      , scVmLedger = vmLedger
      , scSubs = subs
      , scQgaConns = qgaConns
      , scSerialBuffers = serialBufs
      , scMonitorBuffers = monitorBufs
      , scTransferTokens = tokens
      , scTlsConfig = tlsCfg
      , scVmOpLocks = vmOpLocks
      , scVsockLaunchLock = vsockLaunchLock
      }

-- | Run @act@ holding the per-VM lifecycle lock, lazily creating
-- the 'MVar' on first use (mirrors the daemon's
-- 'Corvus.Types.vsockCidLockFor'). Calls for different VMs never
-- contend; the same vmId serialises. Defence in depth — the
-- daemon FSM already prevents conflicting same-VM lifecycle ops.
withVmOpLock :: SessionCap -> Int64 -> IO a -> IO a
withVmOpLock sc vid act = do
  lk <- vmOpLockFor sc vid
  withMVar lk (const act)

vmOpLockFor :: SessionCap -> Int64 -> IO (MVar ())
vmOpLockFor sc vid = do
  m <- readTVarIO (scVmOpLocks sc)
  case Map.lookup vid m of
    Just lk -> pure lk
    Nothing -> do
      lk <- newMVar ()
      -- Race: a concurrent caller may have installed their own
      -- lock first. Re-check inside STM and keep the winner.
      atomically $ do
        m' <- readTVar (scVmOpLocks sc)
        case Map.lookup vid m' of
          Just existing -> pure existing
          Nothing -> do
            writeTVar (scVmOpLocks sc) (Map.insert vid lk m')
            pure lk
