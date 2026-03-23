{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests with multiple VMs running on the same daemon.
-- These tests verify that multiple VMs can coexist and be accessed independently.
--
-- Run with: stack test --test-arguments="--match MultiVmIntegration"
module Corvus.MultiVmIntegrationSpec (spec) where

import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.DSL.Daemon
import Test.Daemon (withTestDaemon)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (withTestDiskSetup)

spec :: Spec
spec = withTestDb $ do
  describe "Multi-VM integration" $ do
    it "two VMs are accessible via SSH simultaneously" $ \env -> do
      withTestDaemon env $ \daemon -> do
        withTestDiskSetup daemon defaultVmConfig True $ \diskId1 cfg1 -> do
          withTestDiskSetup daemon defaultVmConfig True $ \diskId2 cfg2 -> do
            withDaemonVmWithConfig daemon diskId1 cfg1 $ \vm1 -> do
              withDaemonVmWithConfig daemon diskId2 cfg2 $ \vm2 -> do
                -- Verify both VMs are accessible via SSH
                (code1, stdout1, _) <- runInDaemonVm vm1 "hostname"
                code1 `shouldBe` ExitSuccess
                T.strip stdout1 `shouldNotBe` ""

                (code2, stdout2, _) <- runInDaemonVm vm2 "hostname"
                code2 `shouldBe` ExitSuccess
                T.strip stdout2 `shouldNotBe` ""

                -- Verify they are different VMs
                (_, id1, _) <- runInDaemonVm vm1 "cat /etc/machine-id"
                (_, id2, _) <- runInDaemonVm vm2 "cat /etc/machine-id"
                T.strip id1 `shouldNotBe` T.strip id2
