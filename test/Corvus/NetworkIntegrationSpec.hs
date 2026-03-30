{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration tests for virtual networking and namespace manager.
--
-- Run with: stack test --test-arguments="--match NetworkIntegration"
module Corvus.NetworkIntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, try)
import Corvus.Protocol (NetworkInfo (..))
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.Posix.Signals (signalProcess)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (VmConfig (..), defaultVmConfig, withTestVmOnDaemon)
import Test.VM.Daemon (withTestDaemon)
import Test.VM.Rpc (createNetwork, createNetworkWithSubnet, deleteNetwork, showNetwork, startNetwork, stopNetwork)
import Test.VM.Ssh (runInTestVm, runInTestVm_)

spec :: Spec
spec = withTestDb $ do
  describe "Network integration" $ do
    describe "Multi-VM communication" $ do
      it "two VMs are accessible via SSH simultaneously" $ \env -> do
        withTestDaemon env $ \daemon -> do
          withTestVmOnDaemon daemon defaultVmConfig $ \vm1 -> do
            withTestVmOnDaemon daemon defaultVmConfig $ \vm2 -> do
              -- Verify both VMs are accessible via guest agent
              (code1, stdout1, _) <- runInTestVm vm1 "hostname"
              code1 `shouldBe` ExitSuccess
              T.strip stdout1 `shouldNotBe` ""

              (code2, stdout2, _) <- runInTestVm vm2 "hostname"
              code2 `shouldBe` ExitSuccess
              T.strip stdout2 `shouldNotBe` ""

              -- Verify they are different VMs (boot_id is unique per boot)
              (_, id1, _) <- runInTestVm vm1 "cat /proc/sys/kernel/random/boot_id"
              (_, id2, _) <- runInTestVm vm2 "cat /proc/sys/kernel/random/boot_id"
              T.strip id1 `shouldNotBe` T.strip id2

    describe "Namespace manager" $ do
      it "starts namespace manager and dnsmasq for a network with subnet" $ \env -> do
        withTestDaemon env $ \daemon -> do
          bracket
            (do nwId <- createNetworkWithSubnet daemon "test-ns" "10.88.0.0/24"; startNetwork daemon nwId; pure nwId)
            (\nwId -> stopNetwork daemon nwId >> deleteNetwork daemon nwId)
            $ \nwId -> do
              -- Query network state via RPC
              info <- showNetwork daemon nwId

              -- Verify both vde_switch and namespace manager are running
              nwiRunning info `shouldBe` True
              nwiVdeSwitchPid info `shouldSatisfy` isJust
              nwiDnsmasqPid info `shouldSatisfy` isJust

              -- Verify the namespace manager process is actually alive
              let Just nsPid = nwiDnsmasqPid info
              processAlive nsPid `shouldReturn` True

              -- Verify the vde_switch process is actually alive
              let Just vdePid = nwiVdeSwitchPid info
              processAlive vdePid `shouldReturn` True

      it "namespace manager stops cleanly when network is stopped" $ \env -> do
        withTestDaemon env $ \daemon -> do
          nwId <- createNetworkWithSubnet daemon "test-ns-stop" "10.87.0.0/24"
          startNetwork daemon nwId

          -- Get PIDs while running
          info <- showNetwork daemon nwId
          let Just nsPid = nwiDnsmasqPid info
              Just vdePid = nwiVdeSwitchPid info

          -- Stop the network
          stopNetwork daemon nwId

          -- Give processes time to terminate
          threadDelay 500000 -- 500ms

          -- Verify both processes are dead
          processAlive nsPid `shouldReturn` False
          processAlive vdePid `shouldReturn` False

          -- Verify network reports as not running
          infoAfter <- showNetwork daemon nwId
          nwiRunning infoAfter `shouldBe` False
          nwiVdeSwitchPid infoAfter `shouldBe` Nothing
          nwiDnsmasqPid infoAfter `shouldBe` Nothing

          deleteNetwork daemon nwId

      it "network without subnet does not start namespace manager" $ \env -> do
        withTestDaemon env $ \daemon -> do
          bracket
            (do nwId <- createNetwork daemon "test-no-subnet"; startNetwork daemon nwId; pure nwId)
            (\nwId -> stopNetwork daemon nwId >> deleteNetwork daemon nwId)
            $ \nwId -> do
              info <- showNetwork daemon nwId

              -- vde_switch should be running, but no namespace manager
              nwiRunning info `shouldBe` True
              nwiVdeSwitchPid info `shouldSatisfy` isJust
              nwiDnsmasqPid info `shouldBe` Nothing

    describe "Virtual networking" $ do
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
                  -- The VDE interface is the second NIC (eth1)
                  runInTestVm_ vm1 "doas sh -c 'ip addr add 10.0.0.1/24 dev eth1 && ip link set eth1 up'"
                  runInTestVm_ vm2 "doas sh -c 'ip addr add 10.0.0.2/24 dev eth1 && ip link set eth1 up'"

                  -- Verify VM1 can ping VM2
                  (codePing1, _, _) <- runInTestVm vm1 "ping -c 3 -W 5 10.0.0.2"
                  codePing1 `shouldBe` ExitSuccess

                  -- Verify VM2 can ping VM1
                  (codePing2, _, _) <- runInTestVm vm2 "ping -c 3 -W 5 10.0.0.1"
                  codePing2 `shouldBe` ExitSuccess

      it "two VMs get DHCP addresses and communicate over a virtual network" $ \env -> do
        withTestDaemon env $ \daemon -> do
          -- Create and start a virtual network with a subnet (enables DHCP via dnsmasq)
          bracket
            (do nwId <- createNetworkWithSubnet daemon "test-dhcp" "10.99.0.0/24"; startNetwork daemon nwId; pure nwId)
            (\nwId -> stopNetwork daemon nwId >> deleteNetwork daemon nwId)
            $ \nwId -> do
              let config = defaultVmConfig {vmcNetworkId = Just nwId}
              withTestVmOnDaemon daemon config $ \vm1 -> do
                withTestVmOnDaemon daemon config $ \vm2 -> do
                  -- Request DHCP on the VDE interface (eth1)
                  runInTestVm_ vm1 "doas sh -c 'ip link set eth1 up && udhcpc -i eth1 -n -q'"
                  runInTestVm_ vm2 "doas sh -c 'ip link set eth1 up && udhcpc -i eth1 -n -q'"

                  -- Get the DHCP-assigned IP addresses from eth1
                  (_, ip1Raw, _) <- runInTestVm vm1 "ip -4 -o addr show eth1 | awk '{print $4}' | cut -d/ -f1"
                  (_, ip2Raw, _) <- runInTestVm vm2 "ip -4 -o addr show eth1 | awk '{print $4}' | cut -d/ -f1"
                  let ip1 = T.strip ip1Raw
                      ip2 = T.strip ip2Raw

                  -- Verify both VMs got addresses from the subnet
                  ip1 `shouldSatisfy` ("10.99.0." `T.isPrefixOf`)
                  ip2 `shouldSatisfy` ("10.99.0." `T.isPrefixOf`)
                  ip1 `shouldNotBe` ip2

                  -- Verify VM1 can ping VM2
                  (codePing1, _, _) <- runInTestVm vm1 ("ping -c 3 -W 5 " <> ip2)
                  codePing1 `shouldBe` ExitSuccess

                  -- Verify VM2 can ping VM1
                  (codePing2, _, _) <- runInTestVm vm2 ("ping -c 3 -W 5 " <> ip1)
                  codePing2 `shouldBe` ExitSuccess

-- | Check if a process with the given PID is alive by sending signal 0.
processAlive :: Int -> IO Bool
processAlive pid = do
  result <- try $ signalProcess 0 (fromIntegral pid)
  pure $ case result of
    Right () -> True
    Left (_ :: IOException) -> False

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
