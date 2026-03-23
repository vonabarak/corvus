{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests with multiple VMs running on the same daemon.
-- These tests verify that multiple VMs can coexist and be accessed independently.
--
-- Run with: stack test --test-arguments="--match MultiVmIntegration"
module Corvus.MultiVmIntegrationSpec (spec) where

import Control.Exception (bracket)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (VmConfig (..), defaultVmConfig, withTestVmOnDaemon)
import Test.VM.Daemon (withTestDaemon)
import Test.VM.Rpc (createNetwork, deleteNetwork, startNetwork, stopNetwork)
import Test.VM.Ssh (runInTestVm, runInTestVm_)

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

    it "two VMs can communicate over a virtual network" $ \env -> do
      withTestDaemon env $ \daemon -> do
        -- Create and start a virtual network
        bracket
          (do nwId <- createNetwork daemon "test-vde"; startNetwork daemon nwId; pure nwId)
          (\nwId -> stopNetwork daemon nwId >> deleteNetwork daemon nwId)
          $ \nwId -> do
            let config = defaultVmConfig {vmcNetworkId = Just nwId}
            withTestVmOnDaemon daemon config $ \vm1 -> do
              withTestVmOnDaemon daemon config $ \vm2 -> do
                -- Configure static IP addresses on the VDE interfaces
                -- The VDE interface is the second NIC (eth1 on AlmaLinux with virtio-net-pci)
                runInTestVm_ vm1 "sudo nmcli con add type ethernet con-name vde0 ifname eth1 ipv4.method manual ipv4.addresses 10.0.0.1/24"
                runInTestVm_ vm1 "sudo nmcli con up vde0"

                runInTestVm_ vm2 "sudo nmcli con add type ethernet con-name vde0 ifname eth1 ipv4.method manual ipv4.addresses 10.0.0.2/24"
                runInTestVm_ vm2 "sudo nmcli con up vde0"

                -- Verify VM1 can ping VM2
                (codePing1, _, _) <- runInTestVm vm1 "ping -c 3 -W 5 10.0.0.2"
                codePing1 `shouldBe` ExitSuccess

                -- Verify VM2 can ping VM1
                (codePing2, _, _) <- runInTestVm vm2 "ping -c 3 -W 5 10.0.0.1"
                codePing2 `shouldBe` ExitSuccess
