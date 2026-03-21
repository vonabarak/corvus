{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for cloud-init SSH key setup across multiple OS images.
-- Verifies that cloud-init correctly deploys SSH keys on each supported OS,
-- for both UEFI and BIOS boot modes. Reports boot-to-SSH timing for each image.
--
-- Requirements:
--   - QEMU with KVM support
--   - virtiofsd binary
--   - genisoimage or mkisofs
--   - SSH client
--   - PostgreSQL for test database
--   - Network access to download cloud images (first run only)
--
-- Run with: stack test --test-arguments="--match MultiOsIntegration"
module Corvus.MultiOsIntegrationSpec (spec) where

import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.DSL.Daemon
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (withTestVm, withTestVmBios)

-- | VM config with extended SSH timeout for multi-OS test suite
multiOsConfig :: VmConfig
multiOsConfig = defaultVmConfig {vmcWaitSshTimeout = 120}

-- | Verify SSH key auth works, key is deployed, user was created,
-- and privilege escalation to root works (sudo or doas).
verifyVm :: DaemonVm -> IO ()
verifyVm vm = do
  (code, stdout, _) <- runInDaemonVm vm "echo ssh-ok"
  code `shouldBe` ExitSuccess
  T.strip stdout `shouldBe` "ssh-ok"

  (code2, stdout2, _) <- runInDaemonVm vm "cat ~/.ssh/authorized_keys"
  code2 `shouldBe` ExitSuccess
  T.isInfixOf "ssh-" stdout2 `shouldBe` True

  (code3, stdout3, _) <- runInDaemonVm vm "whoami"
  code3 `shouldBe` ExitSuccess
  T.strip stdout3 `shouldBe` "corvus"

  (code4, stdout4, _) <- runInDaemonVm vm "sudo whoami 2>/dev/null || doas whoami"
  code4 `shouldBe` ExitSuccess
  T.strip stdout4 `shouldBe` "root"

spec :: Spec
spec = withTestDb $ do
  describe "Multi-OS cloud-init integration" $ do
    describe "Alpine Linux" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "alpine-3.20-uefi"}) $ verifyVm

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVmBios env (multiOsConfig {vmcOsName = "alpine-3.20-bios"}) $ verifyVm

    describe "AlmaLinux" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "almalinux-10"}) $ verifyVm

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVmBios env (multiOsConfig {vmcOsName = "almalinux-10"}) $ verifyVm

    describe "Ubuntu" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "ubuntu-24.04"}) $ verifyVm

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVmBios env (multiOsConfig {vmcOsName = "ubuntu-24.04"}) $ verifyVm

    describe "Debian" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "debian-12"}) $ verifyVm

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVmBios env (multiOsConfig {vmcOsName = "debian-12"}) $ verifyVm
