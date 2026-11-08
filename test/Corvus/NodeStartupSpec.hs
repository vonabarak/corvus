{-# LANGUAGE OverloadedStrings #-}

module Corvus.NodeStartupSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.STM (atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (forM_, forever, void, when)
import Corvus.Model (GraphicsAdapter (..))
import Corvus.Node.Caps.Session (cleanupDetachedDrive, newSessionCap, withVmOpLock)
import Corvus.Node.Caps.Session.Vm.Startup (forkVmReaper)
import Corvus.Node.GuestAgent.Connection (newGuestAgentConns)
import qualified Corvus.Node.Ledger as L
import Corvus.Node.QemuStartup (waitForVsockOwnership)
import Corvus.Node.Runtime (createVmRuntimeDir, getQmpSocket)
import qualified Corvus.Node.StatusPoller as SP
import Corvus.Node.Transfer (newTokenRegistry)
import Corvus.Node.VmSpec (VmSpec (..))
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBytes
import Supervisors (withSupervisor)
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "QEMU startup failures" $ do
  it "waits for an invalidated unpublished start to finish cleanup" $ do
    ledger <- L.newVmLedger
    _ <- atomically $ L.admitVmStart ledger 7 1 1
    _ <- atomically $ L.fenceVmReset ledger 7 2
    timeout 10000 (atomically $ L.waitForVmStarts ledger 7) `shouldReturn` Nothing
    atomically $ L.clearVmStartReservation ledger 7 1 1
    timeout 1000000 (atomically $ L.waitForVmStarts ledger 7) `shouldReturn` Just ()

  it "includes stderr even when the reader finishes after process exit" $ do
    (_, _, Just stderrHandle, process) <-
      createProcess (proc "sh" ["-c", "echo 'Could not open vde: No such file or directory' >&2; exit 1"]) {std_err = CreatePipe}
    _ <- waitForProcess process
    tailVar <- newTVarIO ""
    doneVar <- newTVarIO False
    void $ forkIO $ do
      threadDelay 50000
      diagnostic <- T.hGetContents stderrHandle
      atomically $ writeTVar tailVar diagnostic
      hClose stderrHandle
      atomically $ writeTVar doneVar True
    result <- waitForVsockOwnership defaultQemuConfig 1 1004 process tailVar doneVar
    result `shouldBe` Left "QEMU exited with status 1: Could not open vde: No such file or directory"

  it "reports the exit status when stderr is empty" $ do
    (_, _, _, process) <- createProcess (proc "sh" ["-c", "exit 2"])
    _ <- waitForProcess process
    tailVar <- newTVarIO ""
    doneVar <- newTVarIO True
    result <- waitForVsockOwnership defaultQemuConfig 1 1004 process tailVar doneVar
    result `shouldBe` Left "QEMU exited with status 2"

  it "leaves QGA startup to the coordinator, then enables periodic probes" $
    withSystemTempDirectory "corvus-startup-poll" $ \runtime -> do
      (_, _, _, process) <- createProcess (proc "true" [])
      _ <- waitForProcess process
      lastExit <- newTVarIO Nothing
      stderrTail <- newTVarIO ""
      stderrDone <- newTVarIO True
      startupError <- newTVarIO Nothing
      startupPending <- newTVarIO True
      workers <- newTVarIO []
      stopRequested <- newTVarIO False
      stopState <- newTVarIO (0, 0)
      let vmSpec = startupSpec
          live =
            L.VmLiveState
              { L.vlsQemuPid = 0
              , L.vlsQemuHandle = process
              , L.vlsVirtiofsd = []
              , L.vlsSwtpm = Nothing
              , L.vlsLastExitCode = lastExit
              , L.vlsStderrTail = stderrTail
              , L.vlsStderrDone = stderrDone
              , L.vlsStartupError = startupError
              , L.vlsStartupPending = startupPending
              , L.vlsSpicePort = 0
              , L.vlsSpec = vmSpec
              , L.vlsWorkers = workers
              , L.vlsStopState = stopState
              , L.vlsStopRequested = stopRequested
              }
          cfg = defaultQemuConfig {qcRuntimeDir = Just runtime, qcBasePath = Just runtime}
      ledger <- L.newVmLedger
      _ <- atomically $ L.admitVmStart ledger 7 1 1
      atomically (L.publishVmStart ledger 7 1 1 live) `shouldReturn` True
      messages <- newIORef []
      conns <- newGuestAgentConns (\_ message -> modifyIORef' messages (message :))
      subs <- SP.newSubscribers
      let probe = SP.dispatchVm cfg conns ledger subs 7
          connected = any (Text.isInfixOf "event=connect_start") <$> readIORef messages
      probe
      connected `shouldReturn` False
      initialLogs <- readIORef messages
      initialLogs `shouldSatisfy` any (Text.isInfixOf "startup_pending=True")
      initialLogs `shouldSatisfy` any (Text.isInfixOf "probe=False")
      initialLogs `shouldSatisfy` any (Text.isInfixOf "runtime_generation=1 lifecycle_revision=1 qemu_pid=0")

      -- A QMP peer that accepts connections but never sends its greeting
      -- must not prevent the startup failure/readiness notification.
      _ <- createVmRuntimeDir cfg 7
      qmpPath <- getQmpSocket cfg 7
      bracket (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol) Socket.close $ \listener -> do
        Socket.bind listener (Socket.SockAddrUnix qmpPath)
        Socket.listen listener 4
        withAsync
          (bracket (fst <$> Socket.accept listener) Socket.close (\_ -> threadDelay 10000000))
          $ \_ -> timeout 4000000 probe `shouldReturn` Just ()
      atomically $ writeTVar startupPending False
      probe
      connected `shouldReturn` True
      readyLogs <- readIORef messages
      readyLogs `shouldSatisfy` any (Text.isInfixOf "startup_pending=False")
      readyLogs `shouldSatisfy` any (Text.isInfixOf "probe=True")
      writeIORef messages []
      -- Even an overlapping or failed stop must fence probes only while active.
      failedStop <- try $ L.withVmStopping live $ do
        readTVarIO stopState `shouldReturn` (1, 1)
        L.withVmStopping live $ readTVarIO stopState `shouldReturn` (2, 2)
        probe
        connected `shouldReturn` False
        atomically $ writeTVar stopRequested True
        ioError (userError "stop failed")
      case failedStop :: Either IOException () of
        Left _ -> pure ()
        Right () -> expectationFailure "stop failure disappeared"
      readTVarIO stopState `shouldReturn` (0, 2)
      readTVarIO stopRequested `shouldReturn` True
      probe
      connected `shouldReturn` True
      writeIORef messages []
      atomically $ writeTVar startupError (Just "startup timed out")
      probe
      connected `shouldReturn` False

      -- Disabling QGA suppresses all dispatched probes, including after startup.
      -- Clear the simulated reaper fence before republishing this fixture.
      atomically $ do
        writeTVar startupError Nothing
        writeTVar stopRequested False
      let disabled = live {L.vlsSpec = vmSpec {vsGuestAgent = False}}
      atomically (L.publishVmStart ledger 7 1 1 disabled) `shouldReturn` True
      writeIORef messages []
      probe
      connected `shouldReturn` False

      _ <- atomically $ L.fenceVmReset ledger 7 2
      atomically (L.publishVmStart ledger 7 1 1 live) `shouldReturn` False
      readTVarIO stopRequested `shouldReturn` True
      -- A rejected publication must stay owned until its process is reaped.
      owned <- atomically $ L.readVms ledger
      length owned `shouldBe` 1

  forM_ [False, True] $ \stopping ->
    it (if stopping then "honours a stop requested during reboot-quirk worker retirement" else "fences reboot-quirk health probes before worker retirement without publishing an exit") $
      withSystemTempDirectory "corvus-respawn-poll" $ \runtime -> withSupervisor $ \sup -> do
        (_, _, _, process) <- createProcess (proc "true" [])
        _ <- waitForProcess process
        lastExit <- newTVarIO Nothing
        stderrTail <- newTVarIO ""
        stderrDone <- newTVarIO True
        startupError <- newTVarIO Nothing
        startupPending <- newTVarIO False
        workers <- newTVarIO []
        stopRequested <- newTVarIO False
        stopState <- newTVarIO (0, 0)
        let vmSpec = startupSpec {vsRebootQuirk = True}
            live =
              L.VmLiveState
                { L.vlsQemuPid = 0
                , L.vlsQemuHandle = process
                , L.vlsVirtiofsd = []
                , L.vlsSwtpm = Nothing
                , L.vlsLastExitCode = lastExit
                , L.vlsStderrTail = stderrTail
                , L.vlsStderrDone = stderrDone
                , L.vlsStartupError = startupError
                , L.vlsStartupPending = startupPending
                , L.vlsSpicePort = 0
                , L.vlsSpec = vmSpec
                , L.vlsWorkers = workers
                , L.vlsStopState = stopState
                , L.vlsStopRequested = stopRequested
                }
            cfg = defaultQemuConfig {qcRuntimeDir = Just runtime, qcBasePath = Just runtime}
        ledger <- L.newVmLedger
        _ <- atomically $ L.admitVmStart ledger 7 1 1
        atomically (L.publishVmStart ledger 7 1 1 live) `shouldReturn` True
        messages <- newIORef []
        conns <- newGuestAgentConns (\_ message -> modifyIORef' messages (message :))
        subs <- SP.newSubscribers
        buffers <- newTVarIO Map.empty
        tokens <- newTokenRegistry
        launch <- newMVar ()
        sc <- newSessionCap "test" sup ledger subs conns buffers buffers tokens Nothing launch
        retiring <- newEmptyMVar
        resume <- newEmptyMVar
        respawned <- newEmptyMVar
        let retire = putMVar retiring () >> takeMVar resume
        forkVmReaper sc vmSpec process (0 :: Int) lastExit stopRequested retire (putMVar respawned ())
        ( do
            timeout 1000000 (takeMVar retiring) `shouldReturn` Just ()
            readTVarIO startupPending `shouldReturn` True
            readTVarIO lastExit `shouldReturn` Nothing
            SP.dispatchVm cfg conns ledger subs 7
            logs <- readIORef messages
            logs `shouldSatisfy` any (Text.isInfixOf "probe=False")
            logs `shouldSatisfy` (not . any (Text.isInfixOf "event=connect_start"))
            logs `shouldSatisfy` (not . any (Text.isInfixOf "event=status_probe_failed"))
            -- The respawn coordinator replaces this entry; it must remain owned
            -- and running rather than reporting a spurious stopped transition.
            current <- atomically $ L.lookupVm ledger 7
            (fmap L.vlsLastExitCode current == Just lastExit) `shouldBe` True
            when stopping $ atomically $ writeTVar stopRequested True
          )
          `finally` putMVar resume ()
        if stopping
          then do
            timeout 1000000 (atomically $ readTVar lastExit >>= check . (== Just 0)) `shouldReturn` Just ()
            tryTakeMVar respawned `shouldReturn` Nothing
          else timeout 1000000 (takeMVar respawned) `shouldReturn` Just ()

  it "yields the VM lock during deferred unplug and stops when the runtime is retired" $
    withSystemTempDirectory "corvus-unplug" $ \runtime -> withSupervisor $ \sup -> do
      (_, _, _, process) <- createProcess (proc "true" [])
      _ <- waitForProcess process
      lastExit <- newTVarIO Nothing
      stderrTail <- newTVarIO ""
      stderrDone <- newTVarIO True
      startupError <- newTVarIO Nothing
      startupPending <- newTVarIO True
      workers <- newTVarIO []
      stopRequested <- newTVarIO False
      stopState <- newTVarIO (0, 0)
      let vmSpec = startupSpec
          live =
            L.VmLiveState
              { L.vlsQemuPid = 0
              , L.vlsQemuHandle = process
              , L.vlsVirtiofsd = []
              , L.vlsSwtpm = Nothing
              , L.vlsLastExitCode = lastExit
              , L.vlsStderrTail = stderrTail
              , L.vlsStderrDone = stderrDone
              , L.vlsStartupError = startupError
              , L.vlsStartupPending = startupPending
              , L.vlsSpicePort = 0
              , L.vlsSpec = vmSpec
              , L.vlsWorkers = workers
              , L.vlsStopState = stopState
              , L.vlsStopRequested = stopRequested
              }
          cfg = defaultQemuConfig {qcRuntimeDir = Just runtime, qcBasePath = Just runtime}

      ledger <- L.newVmLedger
      _ <- atomically $ L.admitVmStart ledger 7 1 1
      atomically (L.publishVmStart ledger 7 1 1 live) `shouldReturn` True
      conns <- newGuestAgentConns (\_ _ -> pure ())
      subs <- SP.newSubscribers
      buffers <- newTVarIO Map.empty
      tokens <- newTokenRegistry
      launch <- newMVar ()
      sc <- newSessionCap "test" sup ledger subs conns buffers buffers tokens Nothing launch
      path <- getQmpSocket cfg 7
      _ <- createVmRuntimeDir cfg 7
      attempts <- newTVarIO (0 :: Int)
      bracket (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol) Socket.close $ \listener -> do
        Socket.bind listener (Socket.SockAddrUnix path)
        Socket.listen listener 1
        let serve = forever $ bracket (fst <$> Socket.accept listener) Socket.close $ \peer -> do
              SocketBytes.sendAll peer "{\"QMP\":{}}\n"
              _ <- SocketBytes.recv peer 4096
              SocketBytes.sendAll peer "{\"return\":{}}\n"
              _ <- SocketBytes.recv peer 4096
              atomically $ modifyTVar' attempts (+ 1)
              SocketBytes.sendAll peer "{\"error\":{\"desc\":\"Node is in use\"}}\n"
        withAsync serve $ \_ ->
          withAsync (cleanupDetachedDrive cfg sc 7 (Just live) "drive-1") $ \worker -> do
            timeout 1000000 (atomically $ readTVar attempts >>= check . (> 0)) `shouldReturn` Just ()
            -- A real foreground operation must obtain the same lock while the
            -- backend remains busy. Removing the runtime also fences retries.
            retired <- timeout 1000000 $ withVmOpLock sc 7 $ do
              _ <- atomically $ L.removeVm ledger 7
              readTVarIO attempts
            retired `shouldSatisfy` (/= Nothing)
            timeout 1000000 (wait worker) `shouldReturn` Just ()
            Just <$> readTVarIO attempts `shouldReturn` retired

-- A cold boot whose first QGA handshake belongs to the startup coordinator.
startupSpec :: VmSpec
startupSpec =
  VmSpec
    { vsVmId = 7
    , vsLifecycleRevision = 1
    , vsRuntimeGeneration = 1
    , vsName = "startup-poll"
    , vsCpuCount = 1
    , vsRam = 134217728
    , vsHeadless = True
    , vsGraphicsAdapter = GraphicsVirtioVga
    , vsGuestAgent = True
    , vsTpm = False
    , vsVsockCid = Nothing
    , vsBalloon = False
    , vsRng = False
    , vsSpicePort = Nothing
    , vsDrives = []
    , vsNetIfs = []
    , vsSharedDirs = []
    , vsAudioDevices = []
    , vsWaitForGuestAgentMs = 120000
    , vsRebootQuirk = False
    , vsSpiceBindAddr = "127.0.0.1"
    , vsLoadFromSavedState = False
    , vsStartPaused = False
    , vsCpuModel = "host"
    }
