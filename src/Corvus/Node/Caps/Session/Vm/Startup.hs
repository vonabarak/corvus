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

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import qualified Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.IO.Class
import Control.Monad.Logger (logInfoN, runStderrLoggingT)
import qualified Control.Monad.Logger
import Corvus.Node.Caps.Session.Utils
  ( SessionCap (..)
  , scQgaConns
  , scVmLedger
  , tshow
  )
import qualified Corvus.Node.GuestAgent as NGA
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.StatusPoller as SP
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.Process as P
import qualified Corvus.Qemu.Config
import qualified Data.Int
import qualified Data.Word
import System.Exit (ExitCode (..))
import System.Posix.Types (CPid (..))
import System.Process
  ( waitForProcess
  )
import qualified System.Process

import Corvus.Node.Caps.Session.Vm.Process (reapSpawnedHelpers)

-- | Stop a VM after a post-spawn prerequisite failed. The ordering matters:
-- suppress reboot-quirk respawn, stop/reap QEMU, publish the errored status
-- while the ledger entry is still visible, then remove local resources.
stopVmAfterStartFailure :: (Control.Monad.IO.Class.MonadIO m, Control.Monad.Logger.MonadLogger m, Integral a) => SessionCap -> Corvus.Qemu.Config.QemuConfig -> Data.Int.Int64 -> a -> System.Process.ProcessHandle -> Control.Concurrent.STM.TVar Bool -> [(Data.Word.Word32, System.Process.ProcessHandle)] -> Maybe (Data.Word.Word32, System.Process.ProcessHandle) -> m ()
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
forkVmReaper :: (Show a) => SessionCap -> VS.VmSpec -> System.Process.ProcessHandle -> a -> Control.Concurrent.STM.TVar (Maybe Int) -> Control.Concurrent.STM.TVar Bool -> IO () -> IO ()
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
