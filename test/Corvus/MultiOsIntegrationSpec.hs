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

  -- Verify privilege escalation to root (sudo on Linux, doas on Alpine).
  -- FreeBSD doesn't ship sudo/doas, so we accept that as a known limitation.
  (code4, stdout4, _) <- runInDaemonVm vm "sudo whoami 2>/dev/null || doas whoami 2>/dev/null"
  if code4 == ExitSuccess
    then T.strip stdout4 `shouldBe` "root"
    else do
      -- If no priv-esc tool is available (e.g. FreeBSD), verify it's an expected OS
      (_, osName, _) <- runInDaemonVm vm "uname -s"
      T.strip osName `shouldSatisfy` (`elem` ["FreeBSD"])

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

    describe "FreeBSD" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "freebsd-14"}) $ verifyVm

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVmBios env (multiOsConfig {vmcOsName = "freebsd-14"}) $ verifyVm
