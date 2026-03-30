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

import Corvus.Types (ServerState (..))
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), VmConfig (..), biosVmConfig, defaultVmConfig, startTestVmAndWait, withTestVm, withTestVmGuestExec)
import Test.VM.Console (connectSerialConsole, consoleExpect, consoleSend)
import Test.VM.Daemon (TestDaemon (..))
import Test.VM.Rpc (editTestVm, runInVm, stopTestVmAndWait)
import Test.VM.Ssh (runInTestVm, runInTestVm_)

spec :: Spec
spec = withTestDb $ do
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
        connectSerialConsole (ssQemuConfig (tdState (tvmDaemon vm))) (tvmId vm) $ \console -> do
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
