{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for SSH key setup across multiple OS images.
-- Verifies that cloud-init correctly deploys SSH keys on each supported OS,
-- for both UEFI and BIOS boot modes.
--
-- Requirements:
--   - QEMU with KVM support
--   - virtiofsd binary
--   - genisoimage or mkisofs
--   - SSH client
--   - PostgreSQL for test database
--   - Network access to download cloud images (first run only)
--
-- Run with: stack test --test-arguments="--match SshKeySetup"
module Corvus.SshKeySetupSpec (spec) where

import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.DSL.Daemon
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (withTestVm, withTestVmBios)

-- | VM config with extended SSH timeout for multi-OS test suite
sshKeyTestConfig :: VmConfig
sshKeyTestConfig = defaultVmConfig {vmcWaitSshTimeout = 120}

-- | Helper: verify SSH key authentication works on a VM
verifySshWorks :: DaemonVm -> IO ()
verifySshWorks vm = do
  (code, stdout, _) <- runInDaemonVm vm "echo ssh-ok"
  code `shouldBe` ExitSuccess
  T.strip stdout `shouldBe` "ssh-ok"

-- | Helper: verify the SSH key is in authorized_keys
verifySshKeyDeployed :: DaemonVm -> IO ()
verifySshKeyDeployed vm = do
  (code, stdout, _) <- runInDaemonVm vm "cat ~/.ssh/authorized_keys"
  code `shouldBe` ExitSuccess
  T.isInfixOf "ssh-" stdout `shouldBe` True

-- | Helper: verify the cloud-init user was created correctly
verifyUserCreated :: DaemonVm -> IO ()
verifyUserCreated vm = do
  (code, stdout, _) <- runInDaemonVm vm "whoami"
  code `shouldBe` ExitSuccess
  T.strip stdout `shouldBe` "corvus"

spec :: Spec
spec = withTestDb $ do
  describe "SSH key setup on Alpine Linux" $ do
    it "works with UEFI boot" $ \env -> do
      let config = sshKeyTestConfig {vmcOsName = "alpine-3.20-uefi"}
      withTestVm env config $ \vm -> do
        verifySshWorks vm
        verifySshKeyDeployed vm
        verifyUserCreated vm

    it "works with BIOS boot" $ \env -> do
      let config = sshKeyTestConfig {vmcOsName = "alpine-3.20-bios"}
      withTestVmBios env config $ \vm -> do
        verifySshWorks vm
        verifySshKeyDeployed vm
        verifyUserCreated vm

  describe "SSH key setup on AlmaLinux" $ do
    it "works with UEFI boot" $ \env -> do
      let config = sshKeyTestConfig {vmcOsName = "almalinux-10"}
      withTestVm env config $ \vm -> do
        verifySshWorks vm
        verifySshKeyDeployed vm
        verifyUserCreated vm

    it "works with BIOS boot" $ \env -> do
      let config = sshKeyTestConfig {vmcOsName = "almalinux-10"}
      withTestVmBios env config $ \vm -> do
        verifySshWorks vm
        verifySshKeyDeployed vm
        verifyUserCreated vm

  describe "SSH key setup on Ubuntu" $ do
    it "works with UEFI boot" $ \env -> do
      let config = sshKeyTestConfig {vmcOsName = "ubuntu-24.04"}
      withTestVm env config $ \vm -> do
        verifySshWorks vm
        verifySshKeyDeployed vm
        verifyUserCreated vm

    it "works with BIOS boot" $ \env -> do
      let config = sshKeyTestConfig {vmcOsName = "ubuntu-24.04"}
      withTestVmBios env config $ \vm -> do
        verifySshWorks vm
        verifySshKeyDeployed vm
        verifyUserCreated vm

  describe "SSH key setup on Debian" $ do
    it "works with UEFI boot" $ \env -> do
      let config = sshKeyTestConfig {vmcOsName = "debian-12"}
      withTestVm env config $ \vm -> do
        verifySshWorks vm
        verifySshKeyDeployed vm
        verifyUserCreated vm

    it "works with BIOS boot" $ \env -> do
      let config = sshKeyTestConfig {vmcOsName = "debian-12"}
      withTestVmBios env config $ \vm -> do
        verifySshWorks vm
        verifySshKeyDeployed vm
        verifyUserCreated vm
