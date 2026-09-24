--------------------------------------------------------------------------------
-- Handler implementations extracted from Corvus.Node.Caps.Session
--
-- This module contains the VM lifecycle, guest exec, status, and related
-- handler implementations for the nodeagent session cap.
--
-- These handlers are called from Corvus.Node.Caps.Session's
-- CGNA.Session'server_ instance.
--------------------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Corvus.Node.Caps.Session.Vm.Lifecycle
  ( encodeVmStopResult
  , handleVmStopGraceful
  , handleVmStopHard
  , handleVmPause
  , handleVmResume
  , handleVmSave
  , outgoingMigrateTimeoutSec
  , pollOutgoingMigrate
  , qmpMigrateWithConnectGrace
  , reapEntryAfterQuit
  , handleDeleteSavedState
  , handleDeleteTpmState
  , sanitiseVmName
  ) where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Streams as CGS
import qualified Capnp.Gen.Vm as CGVm
import Capnp.Rpc (throwFailed)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import qualified Control.Exception as E
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel (..), logDebugN, logInfoN, logWarnN, runStderrLoggingT)
import qualified Corvus.Model as M
import Corvus.Node.Caps.Session.Utils
  ( SessionCap (..)
  , agentQemuConfig
  , captureStderrTail
  , decodeQuiesceMode
  , encodeDiskOpResult
  , forwardPipeToLog
  , monitorBufferCapacity
  , parseFormat
  , pollForExit
  , requireRemovableDrive
  , retryBlockdevDel
  , scMonitorBuffers
  , scOwner
  , scQgaConns
  , scSerialBuffers
  , scSup
  , scTlsConfig
  , scTransferTokens
  , scVmLedger
  , scVmOpLocks
  , serialBufferCapacity
  , stderrTailCapacity
  , tshow
  , vfsBinary
  , waitForFirstQgaPing
  )
import qualified Corvus.Node.CloudInit as NCI
import qualified Corvus.Node.Command as NC
import qualified Corvus.Node.GuestAgent as NGA
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.Qmp as NQ
import qualified Corvus.Node.Runtime as NR
import qualified Corvus.Node.SnapshotLive as NSL
import Corvus.Node.SocketBuffer (flushBuffer, startSocketBufferThread)
import qualified Corvus.Node.StatusPoller as SP
import qualified Corvus.Node.Transfer as NTr
import Corvus.Node.VmSpec (VmAgentState (..), VmGuestExecReq (..), VmSpec (..), VmStopKind (..))
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.Node.VsockCid as VC
import qualified Corvus.Process as P
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Rpc.Streams (callSink)
import Corvus.Types (SocketBufferHandle (..))
import qualified Data.ByteString as BS
import Data.Either (lefts, rights)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Supervisors (Supervisor)
import System.Directory (createDirectoryIfMissing, doesPathExist, getFileSize, removeFile, removePathForcibly, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (BufferMode (..), Handle, hClose, hGetLine, hIsEOF, hSetBuffering)
import System.Posix.Types (CPid (..))
import System.Process
  ( ProcessHandle
  , StdStream (..)
  , createProcess
  , getPid
  , getProcessExitCode
  , proc
  , std_err
  , std_out
  , waitForProcess
  )
import qualified System.Timeout

import Corvus.Node.Caps.Session.Vm.Process (reapVmHelpers)

encodeVmStopResult :: VS.VmStopKind -> Text -> CGNA.Parsed CGNA.VmStopResult
encodeVmStopResult k m =
  CGNA.VmStopResult
    { CGNA.kind = case k of
        VS.VmStopStopped -> CGNA.VmStopKind'stopped
        VS.VmStopAlreadyStopped -> CGNA.VmStopKind'alreadyStopped
        VS.VmStopTimeout -> CGNA.VmStopKind'timeout
        VS.VmStopFailed -> CGNA.VmStopKind'failed
    , CGNA.message = m
    }

-- | Poll QGA every 200 ms up to @timeoutMs@ ms; return 'True' as
-- soon as one ping succeeds, 'False' on timeout. Used by
-- 'doVmStart' to block until the guest agent inside the VM is
-- alive.
-- | Graceful stop: QMP system_powerdown, then poll the reaper's
-- @vlsLastExitCode@ for up to @timeoutSec@. On exit, also reap
-- virtiofsd helpers and drop the ledger entry.
handleVmStopGraceful
  :: SessionCap
  -> Int64
  -> Word32
  -> IO (CGNA.Parsed CGNA.Session'vmStopGraceful'results)
handleVmStopGraceful sc vmId timeoutSec = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> do
      runStderrLoggingT . logDebugN $
        "[nodeagent] vmStopGraceful vm-" <> tshow vmId <> ": not in ledger"
      pure
        CGNA.Session'vmStopGraceful'results
          { CGNA.result = encodeVmStopResult VS.VmStopAlreadyStopped ""
          }
    Just live -> do
      runStderrLoggingT . logInfoN $
        "[nodeagent] vmStopGraceful vm-"
          <> tshow vmId
          <> " pid="
          <> tshow (L.vlsQemuPid live)
          <> " timeout="
          <> tshow timeoutSec
          <> "s"
      -- Record the daemon's stop intent BEFORE signalling QEMU
      -- so the reboot-quirk reaper sees it on a concurrent exit
      -- and skips the auto-restart. Without this flag the
      -- reaper would re-spawn QEMU as soon as the guest's
      -- ACPI handler took it down.
      atomically $ writeTVar (L.vlsStopRequested live) True
      -- Send both shutdown signals: QGA `guest-shutdown` (the guest
      -- runs its own `poweroff` / `shutdown -h now`) and QMP
      -- `system_powerdown` (ACPI power-button). Together they cover
      -- guests with QGA but no acpid (e.g. early cloud-init), guests
      -- with acpid but no QGA, and guests with both. Either one
      -- exiting the QEMU process is good enough.
      _ <- E.try @E.SomeException (NGA.guestShutdown (scQgaConns sc) agentQemuConfig vmId)
      qmpResult <- NQ.qmpShutdown agentQemuConfig vmId
      case qmpResult of
        NQ.QmpSuccess -> pure ()
        NQ.QmpError err -> runStderrLoggingT $ logQmpErr "system_powerdown" err
        NQ.QmpConnectionFailed err ->
          runStderrLoggingT $ logQmpErr "QMP connect" err
      exited <- pollForExit (L.vlsLastExitCode live) (fromIntegral timeoutSec)
      if exited
        then do
          reapVmHelpers live
          _ <- atomically $ L.removeVm (scVmLedger sc) vmId
          -- The QGA chardev is gone with QEMU; drop our cached
          -- per-VM socket so its fd doesn't sit in the conns map
          -- until the agent process restarts.
          NGA.releaseConn (scQgaConns sc) vmId
          pure
            CGNA.Session'vmStopGraceful'results
              { CGNA.result = encodeVmStopResult VS.VmStopStopped ""
              }
        else
          pure
            CGNA.Session'vmStopGraceful'results
              { CGNA.result = encodeVmStopResult VS.VmStopTimeout ""
              }
  where
    logQmpErr ctx err =
      logWarnN
        ("[nodeagent] vmStopGraceful " <> tshow vmId <> " " <> ctx <> ": " <> err)

-- | Hard stop: SIGTERM-then-SIGKILL QEMU + every virtiofsd
-- helper. Drops the ledger entry.
handleVmStopHard
  :: SessionCap -> Int64 -> Maybe Int64 -> IO (CGNA.Parsed CGNA.Session'vmStopHard'results)
handleVmStopHard sc vmId mRevision = do
  fenced <- case mRevision of
    Nothing -> Right <$> atomically (L.removeVm (scVmLedger sc) vmId)
    Just revision -> atomically $ L.fenceVmReset (scVmLedger sc) vmId revision
  mLive <- case fenced of
    Left reason -> throwFailed ("vmStopHard rejected for vmId " <> tshow vmId <> ": " <> reason)
    Right live -> pure live
  case mLive of
    Nothing -> do
      runStderrLoggingT . logDebugN $
        "[nodeagent] vmStopHard vm-" <> tshow vmId <> ": not in ledger"
      pure
        CGNA.Session'vmStopHard'results
          { CGNA.result = encodeVmStopResult VS.VmStopAlreadyStopped ""
          }
    Just live -> do
      runStderrLoggingT . logInfoN $
        "[nodeagent] vmStopHard vm-"
          <> tshow vmId
          <> " pid="
          <> tshow (L.vlsQemuPid live)
      -- Suppress reboot-quirk auto-restart on the impending
      -- QEMU exit. The ledger entry is already evicted above,
      -- but the reaper thread still holds a reference to this
      -- live state's 'vlsStopRequested' TVar.
      atomically $ writeTVar (L.vlsStopRequested live) True
      let qemuLabel = "vm-" <> tshow vmId <> "-qemu"
      stopRes <-
        runStderrLoggingT $
          P.stopProcess
            qemuLabel
            (CPid (fromIntegral (L.vlsQemuPid live)))
            Nothing
            0
            5
      case stopRes of
        P.NotRunning -> pure ()
        _ ->
          runStderrLoggingT $
            P.waitForProcessBounded qemuLabel 5 (L.vlsQemuHandle live)
      reapVmHelpers live
      -- Same QGA-socket cleanup as 'handleVmStopGraceful'; QEMU's
      -- gone and our cached fd points at nothing.
      NGA.releaseConn (scQgaConns sc) vmId
      pure
        CGNA.Session'vmStopHard'results
          { CGNA.result = encodeVmStopResult VS.VmStopStopped ""
          }

-- | QMP @stop@ — freeze CPUs.
handleVmPause
  :: SessionCap -> Int64 -> IO (CGNA.Parsed CGNA.Session'vmPause'results)
handleVmPause sc vmId = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> throwFailed ("vmPause: unknown vmId " <> tshow vmId)
    Just _ -> do
      r <- NQ.qmpStop agentQemuConfig vmId
      case r of
        NQ.QmpSuccess -> pure CGNA.Session'vmPause'results
        NQ.QmpError err -> throwFailed ("qmpStop: " <> err)
        NQ.QmpConnectionFailed err ->
          throwFailed ("qmpStop connect: " <> err)

-- | QMP @cont@ — unpause.
handleVmResume
  :: SessionCap -> Int64 -> IO (CGNA.Parsed CGNA.Session'vmResume'results)
handleVmResume sc vmId = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> throwFailed ("vmResume: unknown vmId " <> tshow vmId)
    Just _ -> do
      r <- NQ.qmpContinue agentQemuConfig vmId
      case r of
        NQ.QmpSuccess -> pure CGNA.Session'vmResume'results
        NQ.QmpError err -> throwFailed ("qmpContinue: " <> err)
        NQ.QmpConnectionFailed err ->
          throwFailed ("qmpContinue connect: " <> err)

-- | Save the VM's running state to disk and terminate QEMU.
--
-- Steps:
--   1. Look up the live VM; refuse if not in the ledger.
--   2. Derive the conventional state-file path from the vmName
--      already recorded in the ledger's spec; @mkdir -p@ the parent
--      so a fresh VM gets its @<basePath>/<vmName>/@ directory.
--   3. Set 'vlsStopRequested' BEFORE issuing migrate so the reaper
--      treats the upcoming QEMU exit as daemon-initiated (no
--      reboot-quirk respawn).
--   4. Issue QMP @migrate file:…@. QEMU starts the file write
--      asynchronously and returns from the command immediately.
--   5. Poll QMP @query-migrate@ every 500 ms (up to 5 min) for
--      @completed@. On 'MigFailed' / 'MigInactive' (the
--      latter shouldn't happen post-migrate but defends against
--      a wedged guest): unlink the partial file, clear
--      'vlsStopRequested' (so the operator can recover the VM via
--      normal stop/reset), and throw with the QMP error.
--   6. Issue QMP @quit@. The reaper observes QEMU exit, sees
--      'vlsStopRequested', and tidies the ledger entry the same
--      way 'handleVmStopHard' does — no respawn, no leftover
--      virtiofsd.
handleVmSave
  :: SessionCap -> Int64 -> IO (CGNA.Parsed CGNA.Session'vmSave'results)
handleVmSave sc vmId = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> throwFailed ("vmSave: unknown vmId " <> tshow vmId)
    Just live -> do
      let cfg = agentQemuConfig
          vmName = VS.vsName (L.vlsSpec live)
      statePath <- NR.getSavedStateFile cfg vmName
      createDirectoryIfMissing True (takeDirectory statePath)
      -- Prevent the reaper from re-spawning when QEMU exits below.
      atomically $ writeTVar (L.vlsStopRequested live) True
      migrateRes <- qmpMigrateWithConnectGrace cfg vmId statePath
      case migrateRes of
        NQ.QmpError err -> do
          atomically $ writeTVar (L.vlsStopRequested live) False
          _ <- E.try @E.SomeException (removeFile statePath)
          throwFailed ("vmSave: qmp migrate failed: " <> err)
        NQ.QmpConnectionFailed err -> do
          atomically $ writeTVar (L.vlsStopRequested live) False
          throwFailed ("vmSave: qmp migrate connect failed: " <> err)
        NQ.QmpSuccess -> do
          pollRes <- pollOutgoingMigrate cfg vmId outgoingMigrateTimeoutSec
          case pollRes of
            Left err -> do
              atomically $ writeTVar (L.vlsStopRequested live) False
              _ <- E.try @E.SomeException (removeFile statePath)
              throwFailed ("vmSave: " <> err)
            Right () -> do
              quitRes <- NQ.qmpQuit cfg vmId
              case quitRes of
                NQ.QmpSuccess -> do
                  -- Reap the QEMU process and clear the ledger
                  -- entry. The reaper sets 'vlsLastExitCode' on its
                  -- own; here we just wait for the exit so the next
                  -- vmStart on this id can re-create the ledger row
                  -- without colliding with the stale one.
                  reapEntryAfterQuit sc vmId live
                  pure CGNA.Session'vmSave'results
                NQ.QmpError err ->
                  throwFailed ("vmSave: qmp quit failed: " <> err)
                NQ.QmpConnectionFailed err ->
                  throwFailed ("vmSave: qmp quit connect failed: " <> err)

-- | 5 minutes. Empirically generous: a 4 GiB VM with a tmpfs
-- destination clocks in under 10 s; this leaves headroom for
-- slow disks and large RAM. Hitting this cap means something is
-- wrong with the guest or storage and the operator should know.
outgoingMigrateTimeoutSec :: Int
outgoingMigrateTimeoutSec = 300

-- | Wait for @query-migrate@ to report @completed@. 500 ms cadence,
-- bounded by the supplied second budget. Returns @Right ()@ on
-- success, @Left reason@ on failure / timeout. Used by both the
-- save path (outgoing) and the load path (incoming) — the QMP
-- shape is the same in both directions.
--
-- Early connect failures (typically "qmp.sock does not exist" right
-- after a fresh spawn while QEMU is still initialising) are
-- absorbed for the first ~5 s; after that they surface as
-- genuine errors. Without this absorber the incoming-migration
-- coordinator races QEMU's chardev creation and tears the VM
-- down before it has a chance to start the file read.
pollOutgoingMigrate :: QemuConfig -> Int64 -> Int -> IO (Either Text ())
pollOutgoingMigrate cfg vmId timeoutSec = go (timeoutSec * 2) (connectGraceTicks :: Int)
  where
    pollIntervalMicros :: Int
    pollIntervalMicros = 500000
    -- 10 ticks * 500 ms = 5 s grace for QEMU to come up.
    connectGraceTicks :: Int
    connectGraceTicks = 10
    go :: Int -> Int -> IO (Either Text ())
    go 0 _ = pure (Left "timed out waiting for migration to complete")
    go ticks grace = do
      r <- NQ.qmpQueryMigrate cfg vmId
      case r of
        Left err
          | grace > 0 && isConnectFailure err -> do
              threadDelay pollIntervalMicros
              go (ticks - 1) (grace - 1)
          | otherwise -> pure (Left ("query-migrate: " <> err))
        Right NQ.MigCompleted -> pure (Right ())
        Right (NQ.MigFailed reason) -> pure (Left ("migration failed: " <> reason))
        Right NQ.MigInactive -> pure (Left "migration never started")
        Right NQ.MigActive -> do
          threadDelay pollIntervalMicros
          go (ticks - 1) 0 -- once QMP responds, no further grace
    isConnectFailure :: Text -> Bool
    isConnectFailure t =
      T.isInfixOf "does not exist" t
        || T.isInfixOf "Connection refused" t
        || T.isInfixOf "No such file or directory" t

-- | Issue @qmpMigrate@, tolerating "qmp.sock does not exist" for ~5 s
-- after a fresh VM start.
--
-- The daemon promotes a no-QGA VM's row from @VmStarting@ to
-- @VmRunning@ as soon as the agent's @vmStart@ RPC returns
-- (see @Corvus.Handlers.Vm@'s no-QGA branch around the
-- @setVmStarted vmId VmRunning pid@ call), which can outrun
-- QEMU's @qmp.sock@ chardev creation by a few hundred ms. A
-- @vm.migrate@ that auto-saves the just-started VM lands inside
-- that window and hits @connect: does not exist@; the failure
-- bubbles up as @QmpConnectionFailed@. Mirrors the ENOENT-absorb
-- pattern 'pollOutgoingMigrate' already uses on the query-migrate
-- side. 10 ticks × 500 ms = 5 s — same budget, same justification.
qmpMigrateWithConnectGrace
  :: QemuConfig
  -> Int64
  -> FilePath
  -> IO NQ.QmpResult
qmpMigrateWithConnectGrace cfg vmId path = go connectGraceTicks
  where
    pollIntervalMicros :: Int
    pollIntervalMicros = 500000
    connectGraceTicks :: Int
    connectGraceTicks = 10
    go :: Int -> IO NQ.QmpResult
    go 0 = NQ.qmpMigrate cfg vmId path
    go grace = do
      r <- NQ.qmpMigrate cfg vmId path
      case r of
        NQ.QmpConnectionFailed _ -> do
          threadDelay pollIntervalMicros
          go (grace - 1)
        _ -> pure r

-- | After QMP @quit@, wait for the QEMU process to actually exit
-- (the reaper's @waitForProcess@ races us), then drop the ledger
-- entry and reap any virtiofsd helpers — mirroring
-- 'handleVmStopHard'\'s tail. Best-effort: a stuck QEMU after
-- @quit@ is rare but not the save path's job to escalate; the
-- 5 s bound matches the existing stop helpers.
reapEntryAfterQuit :: SessionCap -> Int64 -> L.VmLiveState -> IO ()
reapEntryAfterQuit sc vmId live = do
  runStderrLoggingT $
    P.waitForProcessBounded ("vm-" <> tshow vmId <> "-qemu (save)") 5 (L.vlsQemuHandle live)
  reapVmHelpers live
  _ <- atomically $ L.removeVm (scVmLedger sc) vmId
  atomically $ modifyTVar' (scQgaConns sc) (Map.delete vmId)

-- | Unlink @\<basePath\>/\<vmName\>/state.qemu@. Idempotent: a
-- missing file is success. Lives behind a Cap'n Proto method so the
-- daemon doesn't poke at the agent's filesystem directly; the
-- daemon calls this from @handleVmReset@ (operator explicit
-- discard) and @handleVmDelete@ (saved VM being removed). The
-- @vmName@ is sanitised to defend against an over-clever daemon —
-- the daemon already enforces VM-name validity, but the node should
-- not trust the wire blindly.
handleDeleteSavedState
  :: SessionCap
  -> Text
  -> IO (CGNA.Parsed CGNA.Session'deleteSavedState'results)
handleDeleteSavedState _sc vmName = do
  case sanitiseVmName vmName of
    Left err -> throwFailed ("deleteSavedState: " <> err)
    Right safeName -> do
      path <- NR.getSavedStateFile agentQemuConfig safeName
      _ <- E.try @E.SomeException (removeFile path)
      pure CGNA.Session'deleteSavedState'results

-- | Remove a VM's persistent TPM state. Unlike saved-state cleanup,
-- failures are propagated: disabling or deleting a TPM-enabled VM
-- must not silently orphan key material.
handleDeleteTpmState
  :: SessionCap
  -> Text
  -> IO (CGNA.Parsed CGNA.Session'deleteTpmState'results)
handleDeleteTpmState _sc vmName = do
  case sanitiseVmName vmName of
    Left err -> throwFailed ("deleteTpmState: " <> err)
    Right safeName -> do
      path <- NR.getTpmStateDir agentQemuConfig safeName
      exists <- doesPathExist path
      when exists $ removePathForcibly path
      pure CGNA.Session'deleteTpmState'results

-- | Reject names that could escape the @basePath/<vmName>/@
-- directory: empty, absolute, contains @..@, contains a path
-- separator. The daemon already enforces a stricter name policy
-- (@validateName@); this is the agent's safety net.
sanitiseVmName :: Text -> Either Text Text
sanitiseVmName n
  | T.null n = Left "vmName is empty"
  | T.isInfixOf ".." n = Left "vmName contains '..'"
  | T.isInfixOf "/" n = Left "vmName contains '/'"
  | T.isInfixOf "\\" n = Left "vmName contains backslash"
  | T.isInfixOf "\0" n = Left "vmName contains NUL"
  | otherwise = Right n
