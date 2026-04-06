{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests that run commands inside a real VM via the Corvus daemon.
-- These tests require:
--   - QEMU with KVM support
--   - virtiofsd binary
--   - genisoimage or mkisofs
--   - PostgreSQL for test database
--   - Network access to download cloud images (first run only)
--
-- Run with: stack test --test-arguments="--match VmIntegration"
module Corvus.VmIntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (readTVarIO)
import Corvus.Client (showVm, vmStart, vmStop)
import Corvus.Model (VmStatus (..))
import Corvus.Protocol (VmDetails (..))
import Corvus.Server (handleGracefulShutdown)
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), VmConfig (..), biosVmConfig, defaultVmConfig, startTestVmAndWait, withTestVm, withTestVmGuestExec)
import Test.VM.Console (connectSerialConsole, consoleExpect, consoleSend)
import Test.VM.Daemon (TestDaemon (..), withDaemonConnection)
import Test.VM.Rpc (editTestVm, runInVm, stopTestVmAndWait)
import Test.VM.Ssh (runInTestVm, runInTestVm_)

spec :: Spec
spec = do
  vmIntegrationTests
  gracefulShutdownTests

vmIntegrationTests :: Spec
vmIntegrationTests = withTestDb $ do
  describe "VM integration" $ do
    it "guest-exec: runs commands and executes as root" $ \env -> do
      withTestVmGuestExec env defaultVmConfig $ \vm -> do
        -- Echo command
        (code1, stdout1, _) <- runInVm vm "echo hello"
        code1 `shouldBe` ExitSuccess
        T.strip stdout1 `shouldBe` "hello"

        -- Guest agent runs as root
        (code2, stdout2, _) <- runInVm vm "whoami"
        code2 `shouldBe` ExitSuccess
        T.strip stdout2 `shouldBe` "root"

    it "SSH: runs commands, checks OS, reports failures, gets system info" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        -- Echo command
        (code1, stdout1, _) <- runInTestVm vm "echo hello"
        code1 `shouldBe` ExitSuccess
        T.strip stdout1 `shouldBe` "hello"

        -- OS release
        (code2, stdout2, _) <- runInTestVm vm "cat /etc/os-release | grep -i alpine"
        code2 `shouldBe` ExitSuccess
        T.isInfixOf "Alpine" stdout2 `shouldBe` True

        -- Multiple commands (write + read)
        runInTestVm_ vm "echo 'test content' > /tmp/testfile"
        (code3, stdout3, _) <- runInTestVm vm "cat /tmp/testfile"
        code3 `shouldBe` ExitSuccess
        T.strip stdout3 `shouldBe` "test content"

        -- Command failure
        (code4, _, stderr4) <- runInTestVm vm "nonexistent_command_12345"
        code4 `shouldNotBe` ExitSuccess
        T.isInfixOf "not found" stderr4 `shouldBe` True

        -- System information
        (code5, stdout5, _) <- runInTestVm vm "uname -s"
        code5 `shouldBe` ExitSuccess
        T.strip stdout5 `shouldBe` "Linux"

        (code6, _, _) <- runInTestVm vm "cat /proc/meminfo"
        code6 `shouldBe` ExitSuccess

        (code7, _, _) <- runInTestVm vm "cat /proc/mounts"
        code7 `shouldBe` ExitSuccess

    it "can edit CPU and RAM of a stopped VM" $ \env -> do
      let config = defaultVmConfig {vmcCpuCount = 2, vmcRamMb = 2048, vmcWaitSshTimeout = 120}
      withTestVm env config $ \vm -> do
        -- Check initial CPU count
        (code1, stdout1, _) <- runInTestVm vm "nproc"
        code1 `shouldBe` ExitSuccess
        T.strip stdout1 `shouldBe` "2"

        -- Check initial RAM (should be ~2048 MB, check >= 1800 to account for kernel reservation)
        (code2, stdout2, _) <- runInTestVm vm "free -m | awk '/Mem:/{print $2}'"
        code2 `shouldBe` ExitSuccess
        let ramMb1 = read (T.unpack (T.strip stdout2)) :: Int
        ramMb1 `shouldSatisfy` (>= 1800)
        ramMb1 `shouldSatisfy` (<= 2100)

        -- Stop the VM
        let daemon = tvmDaemon vm
        stopTestVmAndWait daemon (tvmId vm) 30

        -- Edit CPU and RAM
        editTestVm daemon (tvmId vm) (Just 4) (Just 4096) Nothing Nothing

        -- Restart the VM and wait for SSH
        startTestVmAndWait vm (vmcWaitSshTimeout config)

        -- Verify new CPU count
        (code3, stdout3, _) <- runInTestVm vm "nproc"
        code3 `shouldBe` ExitSuccess
        T.strip stdout3 `shouldBe` "4"

        -- Verify new RAM
        (code4, stdout4, _) <- runInTestVm vm "free -m | awk '/Mem:/{print $2}'"
        code4 `shouldBe` ExitSuccess
        let ramMb2 = read (T.unpack (T.strip stdout4)) :: Int
        ramMb2 `shouldSatisfy` (>= 3800)
        ramMb2 `shouldSatisfy` (<= 4200)

    it "detects virtio-vga graphics adapter in non-headless VM" $ \env -> do
      -- Use BIOS boot: UEFI NVRAM caches display settings, causing the
      -- headless restart to hang when the VGA device is removed.
      let config = biosVmConfig {vmcHeadless = False}
      withTestVm env config $ \vm -> do
        let daemon = tvmDaemon vm

        -- Check that the virtio-vga device is visible via lshw
        (code, stdout, _) <- runInTestVm vm "lshw -class display"
        code `shouldBe` ExitSuccess
        T.isInfixOf "VGA compatible controller" stdout `shouldBe` True
        T.isInfixOf "Virtio 1.0 GPU" stdout `shouldBe` True

        -- Stop the VM
        stopTestVmAndWait daemon (tvmId vm) 30

        -- Set headless mode
        editTestVm daemon (tvmId vm) Nothing Nothing Nothing (Just True)

        -- Restart the VM and wait for SSH
        startTestVmAndWait vm (vmcWaitSshTimeout config)

        -- Check that VGA adapter is no longer present
        (code2, stdout2, _) <- runInTestVm vm "lshw -class display"
        -- In headless mode there should be no VGA controller
        T.isInfixOf "VGA compatible controller" stdout2 `shouldBe` False

        -- Connect to serial console and verify login prompt is available.
        -- The login prompt was already printed during boot (before we connected),
        -- so send Enter to trigger a fresh one.
        bufMap <- readTVarIO (ssSerialBuffers (tdState (tvmDaemon vm)))
        connectSerialConsole (ssQemuConfig (tdState (tvmDaemon vm))) (tvmId vm) (Just bufMap) $ \console -> do
          consoleSend console ""
          _ <- consoleExpect console "login:" 60
          pure ()

    it "UEFI VM has EFI boot entries" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code, stdout, _) <- runInTestVm vm "efibootmgr"
        code `shouldBe` ExitSuccess
        T.isInfixOf "BootOrder" stdout `shouldBe` True

    it "BIOS VM does not have EFI support" $ \env -> do
      withTestVm env biosVmConfig $ \vm -> do
        (code, _, stderr) <- runInTestVm vm "efibootmgr"
        code `shouldNotBe` ExitSuccess
        T.isInfixOf "EFI variables are not supported" stderr `shouldBe` True

    it "async start/stop works (no --wait)" $ \env -> do
      withTestVmGuestExec env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            vmId = tvmId vm

        -- VM was started by withTestVmGuestExec, verify it's running
        details1 <- getVmDetails daemon vmId
        vdStatus details1 `shouldSatisfy` (\s -> s == VmRunning || s == VmStarting)

        -- Async stop (wait=False) — should return immediately with VmStopping
        stopResp <- withDaemonConnection daemon $ \conn -> vmStop conn (T.pack (show vmId)) False
        case stopResp of
          Right (Right _) -> pure ()
          other -> fail $ "Stop failed: " ++ show other

        -- VM should eventually reach Stopped
        waitForStatus daemon vmId VmStopped 30

        -- Async start (wait=False) — should return immediately
        startResp <- withDaemonConnection daemon $ \conn -> vmStart conn (T.pack (show vmId)) False
        case startResp of
          Right (Right _) -> pure ()
          other -> fail $ "Start failed: " ++ show other

        -- VM should eventually reach Running (after Starting → Running transition)
        waitForStatus daemon vmId VmRunning 120

        -- Verify guest agent works
        (code, stdout, _) <- runInVm vm "echo async-ok"
        code `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "async-ok"

        -- Clean up
        stopTestVmAndWait daemon vmId 30

    it "sync start/stop works (--wait)" $ \env -> do
      withTestVmGuestExec env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            vmId = tvmId vm

        -- Sync stop (wait=True) — should block until Stopped
        stopResp <- withDaemonConnection daemon $ \conn -> vmStop conn (T.pack (show vmId)) True
        case stopResp of
          Right (Right _) -> pure ()
          other -> fail $ "Sync stop failed: " ++ show other

        -- VM should be Stopped immediately (server waited)
        details1 <- getVmDetails daemon vmId
        vdStatus details1 `shouldBe` VmStopped

        -- Sync start (wait=True) — should block until Running
        startResp <- withDaemonConnection daemon $ \conn -> vmStart conn (T.pack (show vmId)) True
        case startResp of
          Right (Right _) -> pure ()
          other -> fail $ "Sync start failed: " ++ show other

        -- VM should be Running immediately (server waited for guest agent)
        details2 <- getVmDetails daemon vmId
        vdStatus details2 `shouldBe` VmRunning

        -- Verify guest agent works
        (code, stdout, _) <- runInVm vm "echo sync-ok"
        code `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "sync-ok"

        -- Clean up
        stopTestVmAndWait daemon vmId 30

-- | Get VM details, failing on error
getVmDetails :: TestDaemon -> Int64 -> IO VmDetails
getVmDetails daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    showVm conn (T.pack (show vmId))
  case result of
    Right (Right (Just details)) -> pure details
    other -> fail $ "Failed to get VM details: " ++ show other

-- | Poll until VM reaches target status, fail on timeout
waitForStatus :: TestDaemon -> Int64 -> VmStatus -> Int -> IO ()
waitForStatus daemon vmId target timeoutSec = go timeoutSec
  where
    go 0 = fail $ "VM did not reach " ++ show target ++ " within " ++ show timeoutSec ++ "s"
    go n = do
      details <- getVmDetails daemon vmId
      if vdStatus details == target
        then pure ()
        else do
          threadDelay 1000000
          go (n - 1)

gracefulShutdownTests :: Spec
gracefulShutdownTests = withTestDb $ do
  describe "Graceful shutdown integration" $ do
    it "graceful shutdown stops running VMs" $ \env -> do
      withTestVmGuestExec env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            vmId = tvmId vm

        -- VM is running, verify
        details1 <- getVmDetails daemon vmId
        vdStatus details1 `shouldBe` VmRunning

        -- Run graceful shutdown (kills all running VMs)
        handleGracefulShutdown (tdState daemon)

        -- VM should be stopped now
        details2 <- getVmDetails daemon vmId
        vdStatus details2 `shouldBe` VmStopped
