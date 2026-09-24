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

module Corvus.Node.Caps.Session.Vm.Start
  ( decodeVmSpec
  , decodeVmDriveSpec
  , decodeVmNetIfSpec
  , decodeVmSharedDirSpec
  , encodeVmRuntimeInfo
  , handleVmStart
  , doVmStart
  , respawnAfterExit
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

import Corvus.Node.Caps.Session.Vm.Lifecycle (outgoingMigrateTimeoutSec, pollOutgoingMigrate)
import Corvus.Node.Caps.Session.Vm.Process (prepareVmRuntime, reapSpawnedHelpers, reapVmHelpers, spawnVmHelpers)
import Corvus.Node.Caps.Session.Vm.Startup (forkVmReaper, stopVmAfterStartFailure, waitForVsockOwnership)

decodeVmSpec :: CGNA.Parsed CGNA.VmSpec -> VS.VmSpec
decodeVmSpec
  CGNA.VmSpec
    { CGNA.vmId = vid
    , CGNA.lifecycleRevision = rev
    , CGNA.runtimeGeneration = gen
    , CGNA.name = n
    , CGNA.cpuCount = c
    , CGNA.ramMb = r
    , CGNA.headless = h
    , CGNA.guestAgent = g
    , CGNA.tpm = tpm
    , CGNA.vsockCid = vc
    , CGNA.hasVsockCid = hvc
    , CGNA.spicePort = sp
    , CGNA.hasSpicePort = hsp
    , CGNA.drives = ds
    , CGNA.netIfs = nis
    , CGNA.sharedDirs = sds
    , CGNA.waitForGuestAgentMs = wms
    , CGNA.rebootQuirk = rq
    , CGNA.spiceBindAddr = sba
    , CGNA.loadFromSavedState = lfs
    , CGNA.cpuModel = cm
    , CGNA.startPaused = sps
    } =
    VS.VmSpec
      { VS.vsVmId = vid
      , VS.vsLifecycleRevision = rev
      , VS.vsRuntimeGeneration = gen
      , VS.vsName = n
      , VS.vsCpuCount = c
      , VS.vsRamMb = r
      , VS.vsHeadless = h
      , VS.vsGuestAgent = g
      , VS.vsTpm = tpm
      , VS.vsVsockCid = if hvc then Just vc else Nothing
      , VS.vsSpicePort = if hsp then Just sp else Nothing
      , VS.vsDrives = map decodeVmDriveSpec ds
      , VS.vsNetIfs = map decodeVmNetIfSpec nis
      , VS.vsSharedDirs = map decodeVmSharedDirSpec sds
      , VS.vsWaitForGuestAgentMs = wms
      , VS.vsRebootQuirk = rq
      , VS.vsSpiceBindAddr = sba
      , VS.vsLoadFromSavedState = lfs
      , VS.vsCpuModel = if T.null cm then "host" else cm
      , VS.vsStartPaused = sps
      }

decodeVmDriveSpec :: CGNA.Parsed CGNA.VmDriveSpec -> VS.VmDriveSpec
decodeVmDriveSpec
  CGNA.VmDriveSpec
    { CGNA.diskFilePath = p
    , CGNA.driveId = did
    , CGNA.format = fmt
    , CGNA.ifKind = ik
    , CGNA.media = md
    , CGNA.readOnly = ro
    , CGNA.cache = ca
    , CGNA.discard = di
    } =
    VS.VmDriveSpec
      { VS.vdsDriveId = did
      , -- \^ an empty wire path encodes a drive with no media (ejected
        -- CD-ROM tray)
        VS.vdsDiskFilePath = if T.null p then Nothing else Just p
      , VS.vdsFormat = fmt
      , VS.vdsIfKind = ik
      , VS.vdsMedia = md
      , VS.vdsReadOnly = ro
      , VS.vdsCache = ca
      , VS.vdsDiscard = di
      }

decodeVmNetIfSpec :: CGNA.Parsed CGNA.VmNetIfSpec -> VS.VmNetIfSpec
decodeVmNetIfSpec
  CGNA.VmNetIfSpec
    { CGNA.ifType = it
    , CGNA.hostDevice = hd
    , CGNA.macAddress = ma
    } =
    VS.VmNetIfSpec
      { VS.vnsIfType = it
      , VS.vnsHostDevice = hd
      , VS.vnsMacAddress = ma
      }

decodeVmSharedDirSpec :: CGNA.Parsed CGNA.VmSharedDirSpec -> VS.VmSharedDirSpec
decodeVmSharedDirSpec
  CGNA.VmSharedDirSpec
    { CGNA.hostPath = hp
    , CGNA.tag = tg
    , CGNA.cache = ca
    , CGNA.readOnly = ro
    } =
    VS.VmSharedDirSpec
      { VS.vssHostPath = hp
      , VS.vssTag = tg
      , VS.vssCache = ca
      , VS.vssReadOnly = ro
      }

decodeVmGuestExecReq :: CGNA.Parsed CGNA.VmGuestExecReq -> VS.VmGuestExecReq
decodeVmGuestExecReq
  CGNA.VmGuestExecReq
    { CGNA.vmId = vid
    , CGNA.path = p
    , CGNA.args = as
    , CGNA.captureOutput = co
    , CGNA.inputData = i
    , CGNA.timeoutSec = t
    } =
    VS.VmGuestExecReq
      { VS.vgeVmId = vid
      , VS.vgePath = p
      , VS.vgeArgs = as
      , VS.vgeCaptureOutput = co
      , VS.vgeInputData = i
      , VS.vgeTimeoutSec = t
      }

encodeVmRuntimeInfo :: L.VmLiveState -> CGNA.Parsed CGNA.VmRuntimeInfo
encodeVmRuntimeInfo live =
  CGNA.VmRuntimeInfo
    { CGNA.qemuPid = fromIntegral (L.vlsQemuPid live) :: Int32
    , CGNA.virtiofsdPids =
        [fromIntegral pid :: Int32 | (pid, _) <- L.vlsVirtiofsd live]
    , CGNA.spicePort = L.vlsSpicePort live
    , CGNA.swtpmPid =
        maybe 0 (fromIntegral . fst) (L.vlsSwtpm live)
    , CGNA.lifecycleRevision = VS.vsLifecycleRevision (L.vlsSpec live)
    , CGNA.runtimeGeneration = VS.vsRuntimeGeneration (L.vlsSpec live)
    }

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

encodeVmAgentStatus
  :: VS.VmAgentState -> Int32 -> Int32 -> CGNA.Parsed CGNA.VmAgentStatus
encodeVmAgentStatus s qpid lec =
  CGNA.VmAgentStatus
    { CGNA.state = case s of
        VS.VmAgentRunning -> CGNA.VmAgentState'running
        VS.VmAgentStopped -> CGNA.VmAgentState'stopped
        VS.VmAgentErrored -> CGNA.VmAgentState'errored
        VS.VmAgentUnknown -> CGNA.VmAgentState'unknown
    , CGNA.qemuPid = qpid
    , CGNA.lastExitCode = lec
    }

encodeVmGuestExecInfo
  :: NGA.GuestExecResult -> CGNA.Parsed CGNA.VmGuestExecInfo
encodeVmGuestExecInfo r = case r of
  NGA.GuestExecSuccess code out err ->
    CGNA.VmGuestExecInfo
      { CGNA.exitCode = fromIntegral code :: Int32
      , CGNA.hasExit = True
      , CGNA.signal = 0
      , CGNA.stdout = TE.encodeUtf8 out
      , CGNA.stderr = TE.encodeUtf8 err
      }
  NGA.GuestExecError err ->
    CGNA.VmGuestExecInfo
      { CGNA.exitCode = -1
      , CGNA.hasExit = False
      , CGNA.signal = 0
      , CGNA.stdout = BS.empty
      , CGNA.stderr = TE.encodeUtf8 err
      }
  NGA.GuestExecConnectionFailed err ->
    CGNA.VmGuestExecInfo
      { CGNA.exitCode = -1
      , CGNA.hasExit = False
      , CGNA.signal = 0
      , CGNA.stdout = BS.empty
      , CGNA.stderr = TE.encodeUtf8 err
      }

-- ---------------------------------------------------------------------------
-- Handler implementations
-- ---------------------------------------------------------------------------

handleVmStart
  :: SessionCap -> VS.VmSpec -> IO (CGNA.Parsed CGNA.Session'vmStart'results)
handleVmStart sc spec =
  case VS.vsVsockCid spec of
    Nothing -> admitAndStart Nothing
    Just cid -> withMVar (scVsockLaunchLock sc) $ \_ -> admitAndStart (Just (fromIntegral cid))
  where
    vmId = VS.vsVmId spec
    ledger = scVmLedger sc
    admitAndStart mCid = do
      admission <-
        atomically $
          L.admitVmStart
            ledger
            vmId
            (VS.vsLifecycleRevision spec)
            (VS.vsRuntimeGeneration spec)
      case admission of
        L.VmStartAccepted mExited -> do
          free <- maybe (pure True) VC.isHostFree mCid
          if not free
            then do
              atomically $
                L.clearVmStartReservation
                  ledger
                  vmId
                  (VS.vsLifecycleRevision spec)
                  (VS.vsRuntimeGeneration spec)
              pure vsockCidBusyResult
            else do
              forM_ mExited reapVmHelpers
              when (isJust mExited) $ NGA.releaseConn (scQgaConns sc) vmId
              doVmStart sc spec
        L.VmStartAlreadyRunning live -> pure $ startedResult live
        L.VmStartRejected reason ->
          throwFailed ("vmStart rejected for vmId " <> tshow vmId <> ": " <> reason)

startedResult :: L.VmLiveState -> CGNA.Parsed CGNA.Session'vmStart'results
startedResult live =
  CGNA.Session'vmStart'results
    { CGNA.result =
        CGNA.VmStartResult
          { CGNA.union' = CGNA.VmStartResult'started (encodeVmRuntimeInfo live)
          }
    }

vsockCidBusyResult :: CGNA.Parsed CGNA.Session'vmStart'results
vsockCidBusyResult =
  CGNA.Session'vmStart'results
    { CGNA.result = CGNA.VmStartResult {CGNA.union' = CGNA.VmStartResult'vsockCidBusy}
    }

-- | Start QEMU and register its runtime before launching asynchronous readiness
-- checks. Keep admission, publication, and synchronous readiness in this order.
doVmStart
  :: SessionCap -> VS.VmSpec -> IO (CGNA.Parsed CGNA.Session'vmStart'results)
doVmStart sc spec = do
  let cfg = agentQemuConfig
  (vmRuntimeDir, monitorSock, qmpSock, serialSock, guestAgentSock, savedStateFile) <-
    prepareVmRuntime cfg spec
  (virtiofsdEntries, swtpmEntry) <- spawnVmHelpers cfg spec vmRuntimeDir
  let command =
        NC.buildQemuCommandFromSpec
          cfg
          spec
          monitorSock
          qmpSock
          serialSock
          guestAgentSock
          vmRuntimeDir
          savedStateFile
  live <- spawnQemu spec command virtiofsdEntries swtpmEntry
  publishStartedVm sc live
  claimVmVsock sc cfg live
  startVmBuffers sc cfg spec serialSock monitorSock
  startVmReaper sc live
  forkStartupCoordinator sc cfg live savedStateFile
  waitForPausedVm cfg spec
  pure $ startedResult live

-- | Build the QEMU process with piped output, cleaning up its helpers if spawn
-- fails or no PID is returned. The no-PID path also performs a bounded reap.
spawnQemu
  :: VS.VmSpec
  -> (FilePath, [String])
  -> [(Word32, ProcessHandle)]
  -> Maybe (Word32, ProcessHandle)
  -> IO L.VmLiveState
spawnQemu spec (binary, args) virtiofsdEntries swtpmEntry = do
  let vmId = VS.vsVmId spec
  runStderrLoggingT $ do
    logInfoN $
      "[nodeagent] vm-" <> tshow vmId <> ": spawning QEMU (" <> T.pack binary <> ")"
    logDebugN $
      "[nodeagent] vm-"
        <> tshow vmId
        <> ": QEMU argv: "
        <> T.pack binary
        <> " "
        <> T.unwords (map (T.pack . show) args)
  spawnResult <-
    E.try @E.SomeException $
      createProcess
        (proc binary args)
          { std_out = CreatePipe
          , std_err = CreatePipe
          }
  case spawnResult of
    Left e -> do
      runStderrLoggingT . logWarnN $
        "[nodeagent] vm-" <> tshow vmId <> ": QEMU spawn failed: " <> T.pack (show e)
      reapSpawnedHelpers virtiofsdEntries swtpmEntry
      throwFailed ("QEMU spawn failed: " <> T.pack (show e))
    Right (_, mStdoutH, mStderrH, qemuPh) -> do
      mPid <- getPid qemuPh
      case mPid of
        Nothing -> do
          runStderrLoggingT . logWarnN $
            "[nodeagent] vm-" <> tshow vmId <> ": QEMU spawn returned no PID"
          reapSpawnedHelpers virtiofsdEntries swtpmEntry
          runStderrLoggingT $
            P.waitForProcessBounded "vm-qemu (no-pid path)" 5 qemuPh
          throwFailed "QEMU spawn returned no PID"
        Just rawPid -> do
          let qemuPidW = fromIntegral rawPid :: Word32
          runStderrLoggingT . logInfoN $
            "[nodeagent] vm-" <> tshow vmId <> ": QEMU started pid=" <> tshow qemuPidW
          newVmLiveState spec qemuPidW qemuPh mStdoutH mStderrH virtiofsdEntries swtpmEntry

-- | Allocate the shared runtime state before publishing it to the ledger.
-- Start draining QEMU's output immediately so its pipes cannot block startup.
newVmLiveState
  :: VS.VmSpec
  -> Word32
  -> ProcessHandle
  -> Maybe Handle
  -> Maybe Handle
  -> [(Word32, ProcessHandle)]
  -> Maybe (Word32, ProcessHandle)
  -> IO L.VmLiveState
newVmLiveState spec qemuPidW qemuPh mStdoutH mStderrH virtiofsdEntries swtpmEntry = do
  lastExitVar <- newTVarIO Nothing
  stderrTailVar <- newTVarIO T.empty
  forwardQemuOutput (VS.vsVmId spec) mStdoutH mStderrH stderrTailVar
  stopRequestedVar <- newTVarIO False
  pure $
    L.VmLiveState
      { L.vlsQemuPid = qemuPidW
      , L.vlsQemuHandle = qemuPh
      , L.vlsVirtiofsd = virtiofsdEntries
      , L.vlsSwtpm = swtpmEntry
      , L.vlsLastExitCode = lastExitVar
      , L.vlsStderrTail = stderrTailVar
      , L.vlsSpicePort = fromMaybe 0 (VS.vsSpicePort spec)
      , L.vlsSpec = spec
      , L.vlsStopRequested = stopRequestedVar
      }

-- | Drain stdout and stderr at debug level to prevent pipe back-pressure.
-- Also retain stderr's tail so an early QEMU exit can report its cause.
forwardQemuOutput :: Int64 -> Maybe Handle -> Maybe Handle -> TVar Text -> IO ()
forwardQemuOutput vmId mStdoutH mStderrH stderrTailVar = do
  let qemuLogLabel = "vm-" <> tshow vmId <> "-qemu"
  forM_ mStdoutH $ \h ->
    void $ forkIO $ forwardPipeToLog (qemuLogLabel <> "/stdout") h
  forM_ mStderrH $ \h ->
    void $
      forkIO $
        captureStderrTail (qemuLogLabel <> "/stderr") h stderrTailVar

-- | Publish only if the lifecycle reservation is still current. A reset that
-- won during spawn must not leave an orphan QEMU process running.
publishStartedVm :: SessionCap -> L.VmLiveState -> IO ()
publishStartedVm sc live = do
  let spec = L.vlsSpec live
      vmId = VS.vsVmId spec
  published <-
    atomically $
      L.publishVmStart
        (scVmLedger sc)
        vmId
        (VS.vsLifecycleRevision spec)
        (VS.vsRuntimeGeneration spec)
        live
  unless published $ do
    stopUnwatchedVm live
    throwFailed ("vmStart rejected by a newer lifecycle fence for vmId " <> tshow vmId)

-- | Stop a process before its reaper is installed, suppressing reboot-quirk
-- respawn and reaping its helpers. Ledger removal is the caller's decision.
stopUnwatchedVm :: L.VmLiveState -> IO ()
stopUnwatchedVm live = do
  atomically $ writeTVar (L.vlsStopRequested live) True
  let qemuLabel = "vm-" <> tshow (VS.vsVmId (L.vlsSpec live)) <> "-qemu"
  _ <-
    runStderrLoggingT $
      P.stopProcess qemuLabel (CPid (fromIntegral (L.vlsQemuPid live))) Nothing 0 5
  reapVmHelpers live

-- | Confirm QMP readiness and ownership while the caller holds the VSOCK
-- launch lock. On failure, stop QEMU and remove only this runtime generation.
claimVmVsock :: SessionCap -> QemuConfig -> L.VmLiveState -> IO ()
claimVmVsock sc cfg live = do
  let spec = L.vlsSpec live
      vmId = VS.vsVmId spec
  forM_ (VS.vsVsockCid spec) $ \cid -> do
    ready <- waitForVsockOwnership cfg vmId cid (L.vlsQemuHandle live)
    case ready of
      Right () -> pure ()
      Left reason -> do
        stopUnwatchedVm live
        _ <-
          atomically $
            L.removeVmIfCurrent
              (scVmLedger sc)
              vmId
              (VS.vsLifecycleRevision spec)
              (VS.vsRuntimeGeneration spec)
        throwFailed ("vmStart failed to claim vsock CID " <> tshow cid <> ": " <> reason)

-- | Buffer chardev output until QEMU exits, then unregister the buffers.
-- Threads wait about a second for sockets to appear. Serial is headless-only
-- (graphical VMs use SPICE); the HMP monitor is always buffered.
startVmBuffers :: SessionCap -> QemuConfig -> VS.VmSpec -> FilePath -> FilePath -> IO ()
startVmBuffers sc cfg spec serialSock monitorSock = do
  let vmId = VS.vsVmId spec
  when (VS.vsHeadless spec) $
    startSocketBufferThread
      cfg
      vmId
      serialSock
      (scSerialBuffers sc)
      serialBufferCapacity
      "serial"
      LevelInfo
  startSocketBufferThread
    cfg
    vmId
    monitorSock
    (scMonitorBuffers sc)
    monitorBufferCapacity
    "monitor"
    LevelInfo

-- | Record ordinary exits, or restart a reboot-quirk VM after a guest-initiated
-- exit. The callback repeats the entire start path: helpers, QEMU, buffers,
-- readiness checks, and a fresh reaper.
startVmReaper :: SessionCap -> L.VmLiveState -> IO ()
startVmReaper sc live =
  forkVmReaper
    sc
    (L.vlsSpec live)
    (L.vlsQemuHandle live)
    (L.vlsQemuPid live)
    (L.vlsLastExitCode live)
    (L.vlsStopRequested live)
    (respawnAfterExit sc (L.vlsSpec live))

-- | Restore incoming state before waiting for QGA, since a postmigrate guest
-- cannot answer until QMP cont resumes it. Run independently of the RPC so
-- one VM's first-ping latency does not serialize starts on the session.
-- Cold boots without a QGA wait need no coordinator; restores without QGA
-- run only the incoming step, and restores with QGA run both in sequence.
forkStartupCoordinator :: SessionCap -> QemuConfig -> L.VmLiveState -> FilePath -> IO ()
forkStartupCoordinator sc cfg live savedStateFile = do
  let spec = L.vlsSpec live
      needIncoming = VS.vsLoadFromSavedState spec
      needGaWait = needsGuestAgentWait spec
  when (needIncoming || needGaWait) $
    void $
      forkIO $ do
        incomingOk <-
          if needIncoming
            then restoreIncomingState sc cfg live savedStateFile
            else pure True
        when (incomingOk && needGaWait) $ awaitGuestAgent sc cfg live

-- | Paused CPUs cannot answer QGA until the caller issues cont, typically
-- after snapshot-load. That caller owns the eventual database transition;
-- the agent must not wait for the guest in the meantime.
needsGuestAgentWait :: VS.VmSpec -> Bool
needsGuestAgentWait spec =
  VS.vsWaitForGuestAgentMs spec > 0 && not (VS.vsStartPaused spec)

-- | Incoming saved-state QEMU starts in postmigrate. Wait for query-migrate
-- completion, then cont, and only then discard the state file. Any failure
-- stops QEMU but leaves the file available for an operator retry or reset.
restoreIncomingState :: SessionCap -> QemuConfig -> L.VmLiveState -> FilePath -> IO Bool
restoreIncomingState sc cfg live savedStateFile = do
  let vmId = VS.vsVmId (L.vlsSpec live)
  pollRes <- pollOutgoingMigrate cfg vmId outgoingMigrateTimeoutSec
  case pollRes of
    Left reason -> failIncomingState sc cfg live ("query-migrate: " <> reason)
    Right () -> do
      contRes <- NQ.qmpContinue cfg vmId
      case contRes of
        NQ.QmpSuccess -> do
          completeIncomingState sc cfg live savedStateFile
          pure True
        NQ.QmpError err ->
          failIncomingState sc cfg live ("cont after incoming-migrate failed: " <> err)
        NQ.QmpConnectionFailed err ->
          failIncomingState sc cfg live ("cont QMP connect failed: " <> err)

-- | Log a restore failure and use the common startup teardown. In particular,
-- do not unlink the saved state: it is still needed for a retry.
failIncomingState :: SessionCap -> QemuConfig -> L.VmLiveState -> Text -> IO Bool
failIncomingState sc cfg live reason = do
  runStderrLoggingT . logWarnN $
    "[nodeagent] vm-"
      <> tshow (VS.vsVmId (L.vlsSpec live))
      <> ": incoming-migration coordinator: "
      <> reason
  stopFailedStartup sc cfg live
  pure False

-- | Unlink consumed state best-effort so a later save can create the file
-- (QEMU migration will not overwrite it). Without a pending QGA wait, publish
-- status here to move the daemon from VmLoading to VmRunning. With QGA, its
-- first ping publishes instead; an earlier ok=false snapshot would be ignored.
completeIncomingState :: SessionCap -> QemuConfig -> L.VmLiveState -> FilePath -> IO ()
completeIncomingState sc cfg live savedStateFile = do
  let spec = L.vlsSpec live
      vmId = VS.vsVmId spec
  runStderrLoggingT . logInfoN $
    "[nodeagent] vm-" <> tshow vmId <> ": loaded saved state and resumed"
  _ <- E.try @E.SomeException (removeFile savedStateFile)
  unless (needsGuestAgentWait spec) $
    SP.dispatchVm cfg (scQgaConns sc) (scVmLedger sc) (scSubs sc) vmId

-- | Race the first QGA ping against the reaper's exit code so early crashes
-- surface in under a second. Success pushes a snapshot that promotes the
-- daemon's VmStarting row to VmRunning; failure tears down and reports error.
awaitGuestAgent :: SessionCap -> QemuConfig -> L.VmLiveState -> IO ()
awaitGuestAgent sc cfg live = do
  let spec = L.vlsSpec live
      vmId = VS.vsVmId spec
  result <-
    waitForFirstQgaPing
      (scQgaConns sc)
      cfg
      vmId
      (L.vlsLastExitCode live)
      (L.vlsStderrTail live)
      (VS.vsWaitForGuestAgentMs spec)
  case result of
    Right () -> do
      runStderrLoggingT . logInfoN $
        "[nodeagent] vm-" <> tshow vmId <> ": first QGA ping landed"
      SP.dispatchVm cfg (scQgaConns sc) (scVmLedger sc) (scSubs sc) vmId
    Left reason -> do
      runStderrLoggingT . logWarnN $
        "[nodeagent] vm-" <> tshow vmId <> ": first QGA ping failed: " <> reason
      stopFailedStartup sc cfg live

-- | Suppress reboot-quirk respawn before stopping QEMU: a failed readiness
-- check is an agent-initiated stop, not a guest reboot, and must not loop.
-- The common teardown pushes the errored snapshot before removing the ledger
-- entry, so buildEntry can read the reaper's exit code and trigger setVmError.
-- Later polling ticks simply omit the removed VM.
stopFailedStartup :: SessionCap -> QemuConfig -> L.VmLiveState -> IO ()
stopFailedStartup sc cfg live =
  runStderrLoggingT $
    stopVmAfterStartFailure
      sc
      cfg
      (VS.vsVmId (L.vlsSpec live))
      (L.vlsQemuPid live)
      (L.vlsQemuHandle live)
      (L.vlsStopRequested live)
      (L.vlsVirtiofsd live)
      (L.vlsSwtpm live)

-- | VSOCK starts already waited for QMP while checking CID ownership.
-- Paused non-VSOCK starts need the same synchronous guarantee before their
-- caller can issue snapshot-load.
waitForPausedVm :: QemuConfig -> VS.VmSpec -> IO ()
waitForPausedVm cfg spec =
  when (VS.vsStartPaused spec && isNothing (VS.vsVsockCid spec)) $
    NQ.waitForQmpReady cfg (VS.vsVmId spec)

-- \| Reboot-quirk re-spawn: after the agent's reaper observed
-- QEMU exit AND the stop wasn't daemon-initiated, reap the
-- stale virtiofsd helpers and call back into 'doVmStart' with
-- the original spec — new QEMU process, new virtiofsd
-- processes, new buffer threads, new reaper, new first-ping
-- watcher.
--
-- We deliberately DO NOT remove the old ledger entry first.
-- The reaper took the quirk branch and left
-- 'vlsLastExitCode' as 'Nothing', so 'handleVmStatus' keeps
-- reporting 'VmAgentRunning' for the stale entry until
-- 'doVmStart's 'insertVm' ('Map.insert') atomically replaces
-- it with the fresh one. Without this overlap the daemon's
-- 1 s 'pollVmUntilExit' poll catches the gap between
-- 'removeVm' and 'insertVm', reads 'VmAgentUnknown', maps
-- that to 'ExitVanished', and permanently flips the VM's DB
-- row to 'VmStopped' (Vm.hs:1456, 1511) — which is exactly
-- the failure mode 'Ledger.hs' lines 50-52 warn against.
--
-- Failure path: if 'doVmStart' throws before its 'insertVm'
-- could replace the stale entry, the daemon would otherwise
-- keep seeing 'VmAgentRunning' on a corpse forever. Write
-- 'Just 1' to the captured old 'vlsLastExitCode' TVar so the
-- next 'handleVmStatus' returns 'VmAgentErrored' and the
-- daemon's monitor reconciles via 'setVmError'. If
-- 'doVmStart' had already replaced the entry before throwing
-- (downstream failure), the captured TVar is no longer
-- referenced from the ledger and the write is a harmless
-- no-op on a garbage-collectable orphan.
respawnAfterExit :: SessionCap -> VS.VmSpec -> IO ()
respawnAfterExit sc spec = do
  let vmId = VS.vsVmId spec
  -- Look up — don't remove. We need the virtiofsd handles for
  -- reaping AND we need the entry to stay in the ledger so
  -- 'handleVmStatus' keeps reporting 'VmAgentRunning' until
  -- 'doVmStart' replaces it.
  mOldLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  -- Reap the now-dead virtiofsd helpers — QEMU's vhost-user
  -- socket closed when QEMU exited, taking the helpers with
  -- it; we just need a bounded 'waitpid' so they don't linger
  -- as zombies.
  forM_ mOldLive $ \live ->
    reapVmHelpers live
  -- Drop the cached QGA socket: the new QEMU re-opens the
  -- chardev under the same path but it's a fresh fd; talking
  -- to the dead socket would give EPIPE on every method.
  atomically $ modifyTVar' (scQgaConns sc) (Map.delete vmId)
  -- Re-execute the full spawn path. Discard the wire result
  -- (no caller is waiting on it — the daemon's original
  -- vmStart already returned long ago). doVmStart's
  -- 'insertVm' atomically replaces the stale entry.
  r <- E.try @E.SomeException $
    case VS.vsVsockCid spec of
      Nothing -> doVmStart sc spec
      Just cid -> withMVar (scVsockLaunchLock sc) $ \_ -> do
        free <- VC.isHostFree (fromIntegral cid)
        unless free $
          throwFailed ("reboot-quirk VSOCK CID is busy: " <> tshow cid)
        doVmStart sc spec
  case r of
    Right _ ->
      runStderrLoggingT . logInfoN $
        "[nodeagent] vm-" <> tshow vmId <> ": reboot-quirk re-spawn succeeded"
    Left e -> do
      forM_ mOldLive $ \live ->
        atomically $ writeTVar (L.vlsLastExitCode live) (Just 1)
      runStderrLoggingT . logWarnN $
        "[nodeagent] vm-"
          <> tshow vmId
          <> ": reboot-quirk re-spawn FAILED: "
          <> T.pack (show e)
          <> "; old entry marked errored so daemon will reconcile"
