{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests that run commands inside a real VM via the Corvus daemon.
-- These tests require:
--   - QEMU with KVM support
--   - virtiofsd binary
--   - genisoimage or mkisofs
--   - SSH client
--   - PostgreSQL for test database
--   - Network access to download cloud images (first run only)
--
-- Run with: stack test --test-arguments="--match VmIntegration"
module Corvus.VmIntegrationSpec (spec) where

import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.DSL.Daemon
import Test.Daemon (withTestDaemon)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (withTestDiskSetup, withTestVm, withTestVmBios)
import Test.VM.Console (connectSerialConsole, consoleExpect, consoleSend)

spec :: Spec
spec = withTestDb $ do
  describe "VM integration" $ do
    -- These tests require a fully functioning cloud-init setup.
    -- They have been verified to work manually but are flaky in CI due to
    -- timing issues with cloud-init user creation.
    it "can run echo command in VM" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code, stdout, _stderr) <- runInDaemonVm vm "echo hello"
        code `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "hello"

    it "can check OS release" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code, stdout, _stderr) <- runInDaemonVm vm "cat /etc/os-release | grep -i alma"
        code `shouldBe` ExitSuccess
        T.isInfixOf "AlmaLinux" stdout `shouldBe` True

    it "can run multiple commands" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        runInDaemonVm_ vm "echo 'test content' > /tmp/testfile"
        (code, stdout, _) <- runInDaemonVm vm "cat /tmp/testfile"
        code `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "test content"

    it "reports command failures correctly" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code, _, stderr) <- runInDaemonVm vm "nonexistent_command_12345"
        code `shouldNotBe` ExitSuccess
        T.isInfixOf "not found" stderr `shouldBe` True

    it "can get system information" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code1, stdout1, _) <- runInDaemonVm vm "uname -s"
        code1 `shouldBe` ExitSuccess
        T.strip stdout1 `shouldBe` "Linux"

        (code2, _, _) <- runInDaemonVm vm "free -m"
        code2 `shouldBe` ExitSuccess

        (code3, _, _) <- runInDaemonVm vm "df -h"
        code3 `shouldBe` ExitSuccess

    it "can use sudo without password" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code, stdout, _) <- runInDaemonVm vm "sudo whoami"
        code `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "root"

    it "can edit CPU and RAM of a stopped VM" $ \env -> do
      let config = defaultVmConfig {vmcCpuCount = 2, vmcRamMb = 2048}
      withTestDaemon env $ \daemon ->
        withTestDiskSetup daemon config True $ \diskId cfg ->
          withDaemonVmWithConfig daemon diskId cfg $ \vm -> do
            -- Check initial CPU count
            (code1, stdout1, _) <- runInDaemonVm vm "nproc"
            code1 `shouldBe` ExitSuccess
            T.strip stdout1 `shouldBe` "2"

            -- Check initial RAM (should be ~2048 MB, check >= 1800 to account for kernel reservation)
            (code2, stdout2, _) <- runInDaemonVm vm "free -m | awk '/Mem:/{print $2}'"
            code2 `shouldBe` ExitSuccess
            let ramMb1 = read (T.unpack (T.strip stdout2)) :: Int
            ramMb1 `shouldSatisfy` (>= 1800)
            ramMb1 `shouldSatisfy` (<= 2100)

            -- Stop the VM
            stopDaemonVmAndWait daemon (dvmId vm) 30

            -- Edit CPU and RAM
            editDaemonVm daemon (dvmId vm) (Just 4) (Just 4096) Nothing Nothing

            -- Restart the VM and wait for SSH
            startDaemonVmAndWait vm (vmcWaitSshTimeout config)

            -- Verify new CPU count
            (code3, stdout3, _) <- runInDaemonVm vm "nproc"
            code3 `shouldBe` ExitSuccess
            T.strip stdout3 `shouldBe` "4"

            -- Verify new RAM
            (code4, stdout4, _) <- runInDaemonVm vm "free -m | awk '/Mem:/{print $2}'"
            code4 `shouldBe` ExitSuccess
            let ramMb2 = read (T.unpack (T.strip stdout4)) :: Int
            ramMb2 `shouldSatisfy` (>= 3800)
            ramMb2 `shouldSatisfy` (<= 4200)

    it "detects virtio-vga graphics adapter in non-headless VM" $ \env -> do
      let config = defaultVmConfig {vmcHeadless = False}
      withTestDaemon env $ \daemon ->
        withTestDiskSetup daemon config True $ \diskId cfg ->
          withDaemonVmWithConfig daemon diskId cfg $ \vm -> do
            -- Check that the virtio-vga device is visible via lshw
            (code, stdout, _) <- runInDaemonVm vm "sudo lshw -class display"
            code `shouldBe` ExitSuccess
            T.isInfixOf "VGA compatible controller" stdout `shouldBe` True
            T.isInfixOf "Virtio 1.0 GPU" stdout `shouldBe` True

            -- Stop the VM
            stopDaemonVmAndWait daemon (dvmId vm) 30

            -- Set headless mode
            editDaemonVm daemon (dvmId vm) Nothing Nothing Nothing (Just True)

            -- Restart the VM and wait for SSH (network still works in headless mode)
            startDaemonVmAndWait vm (vmcWaitSshTimeout cfg)

            -- Check that VGA adapter is no longer present
            (code2, stdout2, _) <- runInDaemonVm vm "sudo lshw -class display"
            -- In headless mode there should be no VGA controller
            T.isInfixOf "VGA compatible controller" stdout2 `shouldBe` False

            -- Connect to serial console and verify login prompt is available.
            -- The login prompt was already printed during boot (before we connected),
            -- so send Enter to trigger a fresh one.
            connectSerialConsole (dvmId vm) $ \console -> do
              consoleSend console ""
              _ <- consoleExpect console "login:" 60
              pure ()

    it "UEFI VM has EFI boot entries" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code, stdout, _) <- runInDaemonVm vm "sudo efibootmgr"
        code `shouldBe` ExitSuccess
        T.isInfixOf "BootOrder" stdout `shouldBe` True

    it "BIOS VM does not have EFI support" $ \env -> do
      withTestVmBios env defaultVmConfig $ \vm -> do
        (code, _, stderr) <- runInDaemonVm vm "sudo efibootmgr"
        code `shouldNotBe` ExitSuccess
        T.isInfixOf "EFI variables are not supported" stderr `shouldBe` True
