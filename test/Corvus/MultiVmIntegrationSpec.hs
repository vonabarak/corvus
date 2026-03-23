{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests with multiple VMs running on the same daemon.
-- These tests verify that multiple VMs can coexist and be accessed independently.
--
-- Run with: stack test --test-arguments="--match MultiVmIntegration"
module Corvus.MultiVmIntegrationSpec (spec) where

import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (defaultVmConfig, withTestVmOnDaemon)
import Test.VM.Daemon (withTestDaemon)
import Test.VM.Ssh (runInTestVm)

spec :: Spec
spec = withTestDb $ do
  describe "Multi-VM integration" $ do
    it "two VMs are accessible via SSH simultaneously" $ \env -> do
      withTestDaemon env $ \daemon -> do
        withTestVmOnDaemon daemon defaultVmConfig $ \vm1 -> do
          withTestVmOnDaemon daemon defaultVmConfig $ \vm2 -> do
            -- Verify both VMs are accessible via SSH
            (code1, stdout1, _) <- runInTestVm vm1 "hostname"
            code1 `shouldBe` ExitSuccess
            T.strip stdout1 `shouldNotBe` ""

            (code2, stdout2, _) <- runInTestVm vm2 "hostname"
            code2 `shouldBe` ExitSuccess
            T.strip stdout2 `shouldNotBe` ""

            -- Verify they are different VMs
            (_, id1, _) <- runInTestVm vm1 "cat /etc/machine-id"
            (_, id2, _) <- runInTestVm vm2 "cat /etc/machine-id"
            T.strip id1 `shouldNotBe` T.strip id2
