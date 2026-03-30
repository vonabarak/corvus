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
module Corvus.CloudInitIntegrationSpec (spec) where

import Control.Monad (when)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), VmConfig (..), cloudVmConfig, withTestVm, withTestVmConsole)
import Test.VM.Console (consoleDrain, consoleExpect, consoleSend)
import Test.VM.Ssh (runInTestVm)

-- | VM config for multi-OS cloud-image tests (uses cloud-init for SSH key deployment)
multiOsConfig :: VmConfig
multiOsConfig = cloudVmConfig {vmcWaitSshTimeout = 120}

-- | Verify SSH key auth works, key is deployed, user was created,
-- and optionally check privilege escalation and qemu-guest-agent.
verifyVm
  :: Bool
  -- ^ Check sudo/doas privilege escalation
  -> Bool
  -- ^ Check qemu-guest-agent is installed
  -> TestVm
  -> IO ()
verifyVm checkPrivEsc checkGuestAgent vm = do
  (code, stdout, _) <- runInTestVm vm "echo ssh-ok"
  code `shouldBe` ExitSuccess
  T.strip stdout `shouldBe` "ssh-ok"

  (code2, stdout2, _) <- runInTestVm vm "cat ~/.ssh/authorized_keys"
  code2 `shouldBe` ExitSuccess
  T.isInfixOf "ssh-" stdout2 `shouldBe` True

  (code3, stdout3, _) <- runInTestVm vm "whoami"
  code3 `shouldBe` ExitSuccess
  T.strip stdout3 `shouldBe` "corvus"

  -- Wait for cloud-init to finish. SSH becomes available before cloud-init
  -- completes modules-final (which runs runcmd and package installation),
  -- so doas config and qemu-guest-agent may not be ready yet.
  when (checkPrivEsc || checkGuestAgent) $ do
    (_, _, _) <- runInTestVm vm "n=0; while [ $n -lt 60 ]; do s=$(cloud-init status 2>/dev/null); case \"$s\" in *done*|*error*|*disabled*) break;; esac; n=$((n+1)); sleep 2; done"
    pure ()

  -- Verify privilege escalation to root (sudo on Linux, doas on Alpine)
  when checkPrivEsc $ do
    (code4, stdout4, _) <- runInTestVm vm "sudo whoami 2>/dev/null || doas whoami 2>/dev/null"
    code4 `shouldBe` ExitSuccess
    T.strip stdout4 `shouldBe` "root"

  -- Verify qemu-guest-agent is installed
  when checkGuestAgent $ do
    (code5, _, _) <- runInTestVm vm "which qemu-ga 2>/dev/null || command -v qemu-ga 2>/dev/null || test -x /usr/sbin/qemu-ga || test -x /usr/local/bin/qemu-ga"
    code5 `shouldBe` ExitSuccess

spec :: Spec
spec = withTestDb $ do
  describe "Multi-OS cloud-init integration" $ do
    describe "Alpine Linux" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "alpine-3.20-uefi"}) (verifyVm True True)

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "alpine-3.20-bios", vmcUefi = False}) (verifyVm True True)

    describe "AlmaLinux" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "almalinux-10"}) (verifyVm True True)

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "almalinux-10", vmcUefi = False}) (verifyVm True True)

    describe "Ubuntu" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "ubuntu-24.04"}) (verifyVm True True)

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "ubuntu-24.04", vmcUefi = False}) (verifyVm True True)

    describe "Debian" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "debian-12"}) (verifyVm True True)

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "debian-12", vmcUefi = False}) (verifyVm True True)

    describe "Gentoo" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "gentoo"}) (verifyVm True False)

    describe "FreeBSD" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "freebsd-14"}) (verifyVm False True)

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "freebsd-14", vmcUefi = False}) (verifyVm False True)

      it "serial console login works" $ \env -> do
        withTestVmConsole env (multiOsConfig {vmcOsName = "freebsd-14"}) $ \console -> do
          -- FreeBSD cloud-init reboots the VM after first boot, so the login
          -- prompt only appears after the second boot cycle (~60s total).
          -- Send Enter to trigger a fresh prompt once getty is ready.
          _ <- consoleExpect console "login:" 90
          _ <- consoleDrain console
          -- Log in as root (no password on FreeBSD console by default)
          consoleSend console "root"
          _ <- consoleExpect console "#" 30
          _ <- consoleDrain console
          -- Run whoami and verify
          consoleSend console "whoami"
          output <- consoleExpect console "root" 10
          T.isInfixOf "root" output `shouldBe` True
