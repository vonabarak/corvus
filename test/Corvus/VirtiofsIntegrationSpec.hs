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
-- Run with: stack test --test-arguments="--match Virtiofs"
module Corvus.VirtiofsIntegrationSpec (spec) where

import Control.Exception (bracket)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (getCanonicalTemporaryDirectory)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (VmConfig (..), defaultVmConfig, withTestVm)
import Test.VM.Ssh (runInTestVm)

spec :: Spec
spec = withTestDb $ do
  describe "Virtiofs integration" $ do
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

      createDirectoryIfMissing True testDir
      writeFile testFile testContent

      bracket
        (pure ())
        (\_ -> removeDirectoryRecursive testDir)
        $ \_ ->
          withTestVm env (defaultVmConfig {vmcSharedDir = Just testDir}) $ \vm -> do
            -- Mount the shared directory
            (code2, _, _) <- runInTestVm vm "doas mkdir -p /mnt/share"
            code2 `shouldBe` ExitSuccess

            (code3, _, _) <-
              runInTestVm
                vm
                "doas mount -t virtiofs share /mnt/share"
            code3 `shouldBe` ExitSuccess

            -- Read the test file
            (code4, stdout4, _) <- runInTestVm vm "cat /mnt/share/testfile.txt"
            code4 `shouldBe` ExitSuccess
            T.strip stdout4 `shouldBe` T.pack testContent
