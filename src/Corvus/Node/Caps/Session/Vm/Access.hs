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
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Corvus.Node.Caps.Session.Vm.Access
  ( decodeVmGuestExecReq
  , encodeVmAgentStatus
  , encodeVmGuestExecInfo
  , handleVmGuestExec
  , handleVmGuestExecStream
  , handleVmStatus
  , handleVmSetSpiceTicket
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
  NGA.GuestExecError err -> failure err
  NGA.GuestExecConnectionFailed err -> failure err
  where
    failure err =
      CGNA.VmGuestExecInfo
        { CGNA.exitCode = -1
        , CGNA.hasExit = False
        , CGNA.signal = 0
        , CGNA.stdout = BS.empty
        , CGNA.stderr = TE.encodeUtf8 err
        }

-- | Execute a command via QGA. The agent locates the QGA socket
-- from the VM's runtime layout; a fresh 'GuestAgentConns' is
-- allocated per call for slice A (slice C introduces an
-- agent-wide persistent cache when the status poller arrives).
handleVmGuestExec
  :: SessionCap
  -> VS.VmGuestExecReq
  -> IO (CGNA.Parsed CGNA.Session'vmGuestExec'results)
handleVmGuestExec sc req = do
  let vmId = VS.vgeVmId req
      conns = scQgaConns sc
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> throwFailed ("vmGuestExec: unknown vmId " <> tshow vmId)
    Just _ -> do
      let cmd =
            T.intercalate
              " "
              (VS.vgePath req : VS.vgeArgs req)
          -- Poll budget: convert vgeTimeoutSec (seconds) into
          -- 100 ms ticks expected by guestExecImpl. A zero value
          -- means "use the agent default" — 60 s, same as the
          -- pre-Phase-4 daemon-side helper.
          maxPolls =
            if VS.vgeTimeoutSec req == 0
              then 600
              else fromIntegral (VS.vgeTimeoutSec req) * 10
      result <-
        if BS.null (VS.vgeInputData req)
          then NGA.guestExecWithTimeout conns agentQemuConfig vmId cmd maxPolls
          else
            NGA.guestExecWithStdin
              conns
              agentQemuConfig
              vmId
              cmd
              (VS.vgeInputData req)
              maxPolls
      pure
        CGNA.Session'vmGuestExec'results
          { CGNA.info = encodeVmGuestExecInfo result
          }

-- | Streaming variant of 'handleVmGuestExec'.
--
-- Pushes incremental stdout / stderr bytes to the supplied
-- 'ByteSink' caps as QGA's @guest-exec-status@ drains them. Both
-- sinks are @end()@-ed on completion (success or failure) so the
-- daemon's line buffer can flush any trailing partial line. The
-- returned 'VmGuestExecInfo' carries the exit code only; stdout /
-- stderr are empty because the bytes already flowed through the
-- sinks.
--
-- Sink-write errors (typically: caller dropped its sink) record a
-- flag locally so the loop can stop polling QGA — there's no
-- point doing more guest-side work when nothing receives the
-- output. The return value in that case is a synthetic
-- 'GuestExecError'.
handleVmGuestExecStream
  :: SessionCap
  -> VS.VmGuestExecReq
  -> C.Client CGS.ByteSink
  -> C.Client CGS.ByteSink
  -> IO (CGNA.Parsed CGNA.Session'vmGuestExecStream'results)
handleVmGuestExecStream sc req stdoutCli stderrCli = do
  let vmId = VS.vgeVmId req
      conns = scQgaConns sc
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> do
      -- End both sinks before bailing so the daemon's
      -- LineBufferSink doesn't sit waiting on a flush that's
      -- never coming.
      endSinkBest stdoutCli
      endSinkBest stderrCli
      throwFailed ("vmGuestExecStream: unknown vmId " <> tshow vmId)
    Just _ -> do
      let cmd =
            T.intercalate
              " "
              (VS.vgePath req : VS.vgeArgs req)
          maxPolls =
            if VS.vgeTimeoutSec req == 0
              then 600
              else fromIntegral (VS.vgeTimeoutSec req) * 10
          mStdin =
            if BS.null (VS.vgeInputData req)
              then Nothing
              else Just (VS.vgeInputData req)
      brokenRef <- newIORef False
      let pushTo client bs = do
            broken <- readIORef brokenRef
            unless broken $ do
              -- Bound each individual write with a 30 s wall
              -- clock. If the daemon's LineBufferSink is somehow
              -- wedged (its onLine cascades to a slow client
              -- sink) the agent's poll loop would otherwise
              -- block on this STM wait indefinitely, eventually
              -- tripping GHC's @BlockedIndefinitelyOnSTM@
              -- detector. Mark broken on timeout so the
              -- remaining drain cycles short-circuit and the
              -- exec still finishes cleanly via the QGA-status
              -- poll.
              r <-
                E.try @E.SomeException $
                  System.Timeout.timeout 30000000 $
                    callSink
                      #write
                      CGS.ByteSink'write'params {CGS.chunk = bs}
                      client
              case r of
                Left _ -> writeIORef brokenRef True
                Right Nothing -> writeIORef brokenRef True
                Right (Just ()) -> pure ()
      result <-
        NGA.guestExecStream
          conns
          agentQemuConfig
          vmId
          cmd
          mStdin
          maxPolls
          (pushTo stdoutCli)
          (pushTo stderrCli)
      endSinkBest stdoutCli
      endSinkBest stderrCli
      pure
        CGNA.Session'vmGuestExecStream'results
          { CGNA.info = encodeVmGuestExecInfo result
          }
  where
    endSinkBest client =
      E.handle (\(_ :: E.SomeException) -> pure ()) $
        void $
          System.Timeout.timeout 10000000 $
            callSink #end CGS.ByteSink'end'params client

handleVmStatus
  :: SessionCap -> Int64 -> IO (CGNA.Parsed CGNA.Session'vmStatus'results)
handleVmStatus sc vmId = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing ->
      pure
        CGNA.Session'vmStatus'results
          { CGNA.status = encodeVmAgentStatus VS.VmAgentUnknown 0 0
          }
    Just live -> do
      mExit <- readTVarIO (L.vlsLastExitCode live)
      let qpid = fromIntegral (L.vlsQemuPid live) :: Int32
      case mExit of
        Nothing ->
          pure
            CGNA.Session'vmStatus'results
              { CGNA.status = encodeVmAgentStatus VS.VmAgentRunning qpid 0
              }
        Just 0 ->
          pure
            CGNA.Session'vmStatus'results
              { CGNA.status = encodeVmAgentStatus VS.VmAgentStopped qpid 0
              }
        Just code ->
          pure
            CGNA.Session'vmStatus'results
              { CGNA.status =
                  encodeVmAgentStatus
                    VS.VmAgentErrored
                    qpid
                    (fromIntegral code)
              }

handleVmSetSpiceTicket
  :: SessionCap
  -> Int64
  -> Text
  -> Word32
  -> IO (CGNA.Parsed CGNA.Session'vmSetSpiceTicket'results)
handleVmSetSpiceTicket sc vmId password ttlSeconds = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> throwFailed ("vmSetSpiceTicket: unknown vmId " <> tshow vmId)
    Just _ -> do
      r1 <- NQ.qmpSetSpicePassword agentQemuConfig vmId password
      case r1 of
        NQ.QmpSuccess -> do
          r2 <-
            NQ.qmpExpireSpicePassword
              agentQemuConfig
              vmId
              (fromIntegral ttlSeconds)
          case r2 of
            NQ.QmpSuccess -> pure CGNA.Session'vmSetSpiceTicket'results
