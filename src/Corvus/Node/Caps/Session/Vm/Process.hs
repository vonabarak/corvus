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

module Corvus.Node.Caps.Session.Vm.Process
  ( prepareVmRuntime
  , spawnVmHelpers
  , spawnVirtiofsdHelper
  , spawnSwtpmHelper
  , spawnSwtpmHelperSafe
  , reapEntryGracefully
  , reapSwtpmEntryGracefully
  , reapSpawnedHelpers
  , reapVmHelpers
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

sanitiseVmName :: Text -> Either Text Text
sanitiseVmName n
  | T.null n = Left "vmName is empty"
  | T.isInfixOf ".." n = Left "vmName contains '..'"
  | T.isInfixOf "/" n = Left "vmName contains '/'"
  | T.isInfixOf "\\" n = Left "vmName contains backslash"
  | T.isInfixOf "\0" n = Left "vmName contains NUL"
  | otherwise = Right n

-- | Create the runtime directory and derive every per-VM socket/state path
-- before starting any child process. This keeps path setup separate from the
-- later rollback-sensitive helper and QEMU spawning stages.
prepareVmRuntime cfg spec = do
  let vmId = VS.vsVmId spec
  _ <- NR.createVmRuntimeDir cfg vmId
  vmRuntimeDir <- NR.getVmRuntimeDir cfg vmId
  monitorSock <- NR.getMonitorSocket cfg vmId
  qmpSock <- NR.getQmpSocket cfg vmId
  serialSock <- NR.getSerialSocket cfg vmId
  guestAgentSock <- NR.getGuestAgentSocket cfg vmId
  savedStateFile <- NR.getSavedStateFile cfg (VS.vsName spec)
  pure (vmRuntimeDir, monitorSock, qmpSock, serialSock, guestAgentSock, savedStateFile)

-- | Start the auxiliary processes required by a VM. Each failure reaps every
-- helper already started by this stage, before QEMU has been launched.
spawnVmHelpers cfg spec vmRuntimeDir = do
  let vmId = VS.vsVmId spec
  virtiofsdResults <-
    mapM (spawnVirtiofsdHelper cfg vmRuntimeDir) (VS.vsSharedDirs spec)
  let virtiofsdEntries = rights virtiofsdResults
      virtiofsdErrs = lefts virtiofsdResults
  unless (null virtiofsdErrs) $ do
    forM_ virtiofsdEntries reapEntryGracefully
    throwFailed
      ( "virtiofsd spawn failed for vmId "
          <> tshow vmId
          <> ": "
          <> T.intercalate "; " virtiofsdErrs
      )
  swtpmResult <-
    if VS.vsTpm spec
      then fmap Just <$> spawnSwtpmHelper cfg vmId (VS.vsName spec)
      else pure (Right Nothing)
  swtpmEntry <- case swtpmResult of
    Left err -> do
      forM_ virtiofsdEntries reapEntryGracefully
      throwFailed ("swtpm spawn failed for vmId " <> tshow vmId <> ": " <> err)
    Right entry -> pure entry
  pure (virtiofsdEntries, swtpmEntry)

-- | Spawn one virtiofsd helper for a shared dir. Returns the PID
-- + 'ProcessHandle' on success, or an error string on failure
-- (caller cleans up any sibling spawns).
spawnVirtiofsdHelper
  :: QemuConfig
  -> FilePath
  -> VS.VmSharedDirSpec
  -> IO (Either Text (Word32, ProcessHandle))
spawnVirtiofsdHelper cfg vmRuntimeDir d = do
  let tag = T.unpack (VS.vssTag d)
      socketPath = vmRuntimeDir <> "/virtiofsd-" <> tag <> ".sock"
      binary = vfsBinary cfg
      baseArgs =
        [ "--socket-path=" <> socketPath
        , "--shared-dir=" <> T.unpack (VS.vssHostPath d)
        , "--cache=" <> T.unpack (VS.vssCache d)
        , "--sandbox=none"
        ]
      args
        | VS.vssReadOnly d = baseArgs <> ["--readonly"]
        | otherwise = baseArgs
  runStderrLoggingT $ do
    logInfoN $
      "[nodeagent] spawning virtiofsd tag="
        <> T.pack tag
        <> " host-path="
        <> VS.vssHostPath d
    logDebugN $
      "[nodeagent] virtiofsd argv: "
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
        "[nodeagent] virtiofsd tag=" <> T.pack tag <> " spawn failed: " <> T.pack (show e)
      pure $ Left ("virtiofsd " <> T.pack tag <> ": " <> T.pack (show e))
    Right (_, mStdoutH, mStderrH, ph) -> do
      -- Forward virtiofsd's stdout/stderr to the agent log at debug
      -- level. Leaving the pipes unread blocks virtiofsd once the
      -- kernel buffer fills, and — combined with skipping
      -- 'waitForProcess' on the reap path — was the root cause of
      -- the lingering @[virtiofsd] <defunct>@ zombies after VM
      -- teardown.
      let vfsLogLabel = "virtiofsd-" <> T.pack tag
      forM_ mStdoutH $ \h ->
        void $ forkIO $ forwardPipeToLog (vfsLogLabel <> "/stdout") h
      forM_ mStderrH $ \h ->
        void $ forkIO $ forwardPipeToLog (vfsLogLabel <> "/stderr") h
      mPid <- getPid ph
      case mPid of
        Nothing -> do
          runStderrLoggingT . logWarnN $
            "[nodeagent] virtiofsd tag=" <> T.pack tag <> ": no PID after spawn"
          runStderrLoggingT $
            P.waitForProcessBounded
              ("virtiofsd " <> T.pack tag <> " (no-pid)")
              5
              ph
          pure $ Left ("virtiofsd " <> T.pack tag <> ": no PID")
        Just rawPid -> do
          runStderrLoggingT . logInfoN $
            "[nodeagent] virtiofsd tag=" <> T.pack tag <> " started pid=" <> tshow rawPid
          ready <- P.waitForSocketFile socketPath 5000
          if ready
            then pure $ Right (fromIntegral rawPid, ph)
            else do
              let partialLabel = "virtiofsd-partial-" <> T.pack tag
              _ <-
                runStderrLoggingT $
                  P.stopProcess
                    partialLabel
                    (CPid (fromIntegral rawPid))
                    Nothing
                    0
                    3
              -- Always reap, even when stopProcess saw NotRunning
              -- (zombie state still requires our @waitpid@) — see
              -- 'reapEntryGracefully' for the full rationale.
              runStderrLoggingT $ P.waitForProcessBounded partialLabel 5 ph
              pure $ Left ("virtiofsd " <> T.pack tag <> ": socket never appeared")

-- | Spawn the per-VM TPM 2.0 emulator in the foreground so the
-- nodeagent owns its process handle exactly as it owns virtiofsd.
spawnSwtpmHelper
  :: QemuConfig
  -> Int64
  -> Text
  -> IO (Either Text (Word32, ProcessHandle))
spawnSwtpmHelper cfg vmId vmName =
  case sanitiseVmName vmName of
    Left err -> pure $ Left ("unsafe VM name: " <> err)
    Right safeName -> spawnSwtpmHelperSafe cfg vmId safeName

spawnSwtpmHelperSafe
  :: QemuConfig
  -> Int64
  -> Text
  -> IO (Either Text (Word32, ProcessHandle))
spawnSwtpmHelperSafe cfg vmId vmName = do
  socketPath <- NR.getSwtpmSocket cfg vmId
  stateDir <- NR.createTpmStateDir cfg vmName
  stale <- doesPathExist socketPath
  when stale (removeFile socketPath)
  let binary = qcSwtpmBinary cfg
      args =
        [ "socket"
        , "--tpm2"
        , "--tpmstate"
        , "dir=" <> stateDir <> ",mode=0600"
        , "--ctrl"
        , "type=unixio,path=" <> socketPath <> ",mode=0600"
        , "--terminate"
        ]
      label = "swtpm vm-" <> tshow vmId
  runStderrLoggingT $ do
    logInfoN $ "[nodeagent] vm-" <> tshow vmId <> ": spawning swtpm"
    logDebugN $
      "[nodeagent] swtpm argv: "
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
        "[nodeagent] vm-" <> tshow vmId <> ": swtpm spawn failed: " <> T.pack (show e)
      pure $ Left (T.pack (show e))
    Right (_, mStdoutH, mStderrH, ph) -> do
      forM_ mStdoutH $ \h ->
        void $ forkIO $ forwardPipeToLog (label <> "/stdout") h
      forM_ mStderrH $ \h ->
        void $ forkIO $ forwardPipeToLog (label <> "/stderr") h
      mPid <- getPid ph
      case mPid of
        Nothing -> do
          runStderrLoggingT $ P.waitForProcessBounded (label <> " (no-pid)") 5 ph
          pure $ Left "spawn returned no PID"
        Just rawPid -> do
          ready <- P.waitForSocketFile socketPath 5000
          if ready
            then do
              runStderrLoggingT . logInfoN $
                "[nodeagent] vm-" <> tshow vmId <> ": swtpm started pid=" <> tshow rawPid
              pure $ Right (fromIntegral rawPid, ph)
            else do
              let entry = (fromIntegral rawPid, ph)
              reapSwtpmEntryGracefully entry
              pure $ Left "control socket never appeared"

-- | Best-effort termination + reap of a virtiofsd helper.
--
-- Always call 'waitForProcessBounded' afterwards — including when
-- 'stopProcess' returned 'NotRunning'. That branch fires when the
-- process is already in zombie state (@/proc/<pid>/status@ shows
-- @State: Z@) or its @/proc@ entry has vanished, and in the former
-- case the kernel is still waiting on its parent (us) to
-- @waitpid()@ the entry. Skipping the wait — the old behaviour —
-- left every cleanly-exiting virtiofsd around as
-- @[virtiofsd] <defunct>@. The bounded wait can't deadlock the
-- caller: 'waitForProcessBounded' caps its own wallclock and
-- abandons the handle on timeout.
reapEntryGracefully :: (Word32, ProcessHandle) -> IO ()
reapEntryGracefully (pid, ph) = do
  let label = "virtiofsd pid=" <> tshow pid
  _ <-
    runStderrLoggingT $
      P.stopProcess label (CPid (fromIntegral pid)) Nothing 0 3
  runStderrLoggingT $ P.waitForProcessBounded label 5 ph

-- | Best-effort termination + reap of the per-VM swtpm helper.
reapSwtpmEntryGracefully :: (Word32, ProcessHandle) -> IO ()
reapSwtpmEntryGracefully (pid, ph) = do
  let label = "swtpm pid=" <> tshow pid
  _ <-
    runStderrLoggingT $
      P.stopProcess label (CPid (fromIntegral pid)) Nothing 0 3
  runStderrLoggingT $ P.waitForProcessBounded label 5 ph

reapSpawnedHelpers
  :: [(Word32, ProcessHandle)]
  -> Maybe (Word32, ProcessHandle)
  -> IO ()
reapSpawnedHelpers virtiofsdEntries swtpmEntry = do
  forM_ virtiofsdEntries reapEntryGracefully
  forM_ swtpmEntry reapSwtpmEntryGracefully

reapVmHelpers :: L.VmLiveState -> IO ()
reapVmHelpers live =
  reapSpawnedHelpers (L.vlsVirtiofsd live) (L.vlsSwtpm live)
