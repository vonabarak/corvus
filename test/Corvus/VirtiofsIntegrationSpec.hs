{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration tests for virtiofs shared directories.
-- These tests verify that virtiofsd is properly managed by the daemon
-- and that shared directories are accessible from within VMs.
--
-- Requirements:
--   - QEMU with KVM support
--   - virtiofsd binary
--   - Debian cloud image
--   - PostgreSQL for test database
--
-- Run with: cabal test --test-arguments="--match Virtiofs"
module Corvus.VirtiofsIntegrationSpec (spec) where

import Control.Exception (bracket)
import Corvus.Client (DiskResult (..), diskRegister)
import Corvus.Model (DriveFormat (..))
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (getCanonicalTemporaryDirectory)
import Test.DSL.Daemon
import Test.Daemon (startTestDaemon, stopTestDaemon, withDaemonConnection)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Image (createOverlay, defaultImageConfig, ensureBaseImage, removeOverlay)

spec :: Spec
spec = withTestDb $ do
  describe "Virtiofs integration through daemon (requires cloud-init support)" $ do
    -- This test requires a fully functioning cloud-init setup.
    -- It has been verified to work manually but is flaky in CI due to
    -- timing issues with cloud-init user creation.
    it "can access shared directory from VM via virtiofs" $ \env -> do
      -- Create a temporary directory with a test file
      sysTmp <- getCanonicalTemporaryDirectory
      uuid <- nextRandom
      let testDir = sysTmp </> ("virtiofs-test-" <> T.unpack (T.take 8 (toText uuid)))
          testFile = testDir </> "testfile.txt"
          testContent = "UUID:" <> T.unpack (toText uuid)
          overlayPath = sysTmp </> ("virtiofs-vm-" <> T.unpack (T.take 8 (toText uuid)) <> ".qcow2")

      createDirectoryIfMissing True testDir
      writeFile testFile testContent

      -- Start the daemon
      daemon <- startTestDaemon env

      -- Ensure base cloud image exists
      imageResult <- ensureBaseImage defaultImageConfig
      basePath <- case imageResult of
        Left err -> fail $ "Failed to get base image: " <> T.unpack err
        Right path -> pure path

      -- Create overlay for this test
      overlayResult <- createOverlay basePath overlayPath
      case overlayResult of
        Left err -> fail $ "Failed to create overlay: " <> T.unpack err
        Right _ -> pure ()

      bracket
        (pure ())
        ( \_ -> do
            stopTestDaemon daemon
            removeOverlay overlayPath
            removeDirectoryRecursive testDir
        )
        ( \_ -> do
            -- Register the overlay as a disk in the daemon
            diskResult <- withDaemonConnection daemon $ \conn ->
              diskRegister conn "debian-cloud" (T.pack overlayPath) FormatQcow2 Nothing

            diskId <- case diskResult of
              Left err -> fail $ "Connection error: " <> show err
              Right (Left err) -> fail $ "Failed to register disk: " <> show err
              Right (Right (DiskCreated dId)) -> pure dId
              Right (Right other) -> fail $ "Unexpected response: " <> show other

            -- Run the test with a daemon-managed VM
            withDaemonVm daemon diskId (Just testDir) $ \vm -> do
              -- Mount the shared directory
              (code2, _, _) <- runInDaemonVm vm "sudo mkdir -p /mnt/share"
              code2 `shouldBe` ExitSuccess

              (code3, _, _) <-
                runInDaemonVm
                  vm
                  "sudo mount -t virtiofs share /mnt/share"
              code3 `shouldBe` ExitSuccess

              -- Read the test file
              (code4, stdout4, _) <- runInDaemonVm vm "cat /mnt/share/testfile.txt"
              code4 `shouldBe` ExitSuccess
              T.strip stdout4 `shouldBe` T.pack testContent
        )
