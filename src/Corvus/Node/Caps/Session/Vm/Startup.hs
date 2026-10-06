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

module Corvus.Node.Caps.Session.Vm.Startup
  ( stopVmAfterStartFailure
  , forkVmReaper
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

import Corvus.Node.Caps.Session.Vm.Process (reapSpawnedHelpers)

-- | Stop a VM after a post-spawn prerequisite failed. The ordering matters:
-- suppress reboot-quirk respawn, stop/reap QEMU, publish the errored status
-- while the ledger entry is still visible, then remove local resources.
stopVmAfterStartFailure sc cfg vmId qemuPidW qemuPh stopRequestedVar virtiofsdEntries swtpmEntry = do
  liftIO $ atomically $ writeTVar stopRequestedVar True
  let qemuLabel = "vm-" <> tshow vmId <> "-qemu"
  stopRes <-
    P.stopProcess qemuLabel (CPid (fromIntegral qemuPidW)) Nothing 0 5
  case stopRes of
    P.NotRunning -> pure ()
    _ -> P.waitForProcessBounded qemuLabel 5 qemuPh
  liftIO $
    SP.dispatchVm cfg (scQgaConns sc) (scVmLedger sc) (scSubs sc) vmId
  _ <- liftIO $ atomically $ L.removeVm (scVmLedger sc) vmId
  liftIO $ reapSpawnedHelpers virtiofsdEntries swtpmEntry
  liftIO $ NGA.releaseConn (scQgaConns sc) vmId

-- | Watch QEMU independently of the start RPC. Guest-initiated exits use the
-- reboot-quirk path; all other exits become visible to status polling and
-- release the persistent QGA connection.
forkVmReaper sc spec qemuPh qemuPidW lastExitVar stopRequestedVar respawn =
  void $ forkIO $ do
    r <- E.try @E.SomeException (waitForProcess qemuPh)
    let vmId = VS.vsVmId spec
        code = case r of
          Right ExitSuccess -> 0
          Right (ExitFailure n) -> n
          Left _ -> 1
    stopReq <- readTVarIO stopRequestedVar
    if VS.vsRebootQuirk spec && not stopReq
      then do
        runStderrLoggingT . logInfoN $
          "[nodeagent] vm-"
            <> tshow vmId
            <> ": QEMU exited pid="
            <> tshow qemuPidW
            <> " code="
            <> tshow code
            <> "; reboot-quirk → re-spawning"
        respawn
      else do
        atomically $ writeTVar lastExitVar (Just code)
        runStderrLoggingT . logInfoN $
          "[nodeagent] vm-"
            <> tshow vmId
            <> ": QEMU exited pid="
            <> tshow qemuPidW
            <> " code="
            <> tshow code
        NGA.releaseConn (scQgaConns sc) vmId
