{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration tests for virtual networking with bridge/TAP in namespace.
--
-- Run with: stack test --test-arguments="--match NetworkIntegration"
module Corvus.NetworkIntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Corvus.Protocol (NetworkInfo (..))
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (VmConfig (..), defaultVmConfig, withTestVmOnDaemon)
import Test.VM.Daemon (withTestDaemon)
import Test.VM.Rpc (createNetwork, createNetworkWithNat, createNetworkWithSubnet, deleteNetwork, showNetwork, startNetwork, stopNetwork)
import Test.VM.Ssh (runInTestVm, runInTestVm_)

spec :: Spec
spec = withTestDb $ do
  describe "Network integration" $ do
    describe "Multi-VM communication" $ do
      it "two VMs are accessible via SSH simultaneously" $ \env -> do
        withTestDaemon env $ \daemon -> do
          withTestVmOnDaemon daemon defaultVmConfig {vmcForceTcpSsh = True} $ \vm1 -> do
            withTestVmOnDaemon daemon defaultVmConfig {vmcForceTcpSsh = True} $ \vm2 -> do
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

    describe "Network namespace" $ do
      it "starts dnsmasq for a network with DHCP enabled" $ \env -> do
        withTestDaemon env $ \daemon -> do
          bracket
            (do nwId <- createNetworkWithSubnet daemon "test-ns" "10.88.0.0/24"; startNetwork daemon nwId; pure nwId)
            (\nwId -> stopNetwork daemon nwId >> deleteNetwork daemon nwId)
            $ \nwId -> do
              -- Query network state via RPC
              info <- showNetwork daemon nwId

              -- Verify network is running with DHCP
              nwiRunning info `shouldBe` True
              nwiDhcp info `shouldBe` True
              nwiDnsmasqPid info `shouldSatisfy` isJust

              -- Verify the dnsmasq process is actually alive
              let Just dnsPid = nwiDnsmasqPid info
              processAlive dnsPid `shouldReturn` True

      it "dnsmasq stops cleanly when network is stopped" $ \env -> do
        withTestDaemon env $ \daemon -> do
          nwId <- createNetworkWithSubnet daemon "test-ns-stop" "10.87.0.0/24"
          startNetwork daemon nwId

          -- Get PID while running
          info <- showNetwork daemon nwId
          let Just dnsPid = nwiDnsmasqPid info

          -- Stop the network
          stopNetwork daemon nwId

          -- Wait for process to terminate (up to 5 seconds)
          waitForProcessExit dnsPid 50

          -- Verify process is dead
          processAlive dnsPid `shouldReturn` False

          -- Verify network reports as not running
          infoAfter <- showNetwork daemon nwId
          nwiRunning infoAfter `shouldBe` False
          nwiDnsmasqPid infoAfter `shouldBe` Nothing

          deleteNetwork daemon nwId

      it "network without DHCP does not start dnsmasq" $ \env -> do
        withTestDaemon env $ \daemon -> do
          bracket
            (do nwId <- createNetwork daemon "test-no-dhcp"; startNetwork daemon nwId; pure nwId)
            (\nwId -> stopNetwork daemon nwId >> deleteNetwork daemon nwId)
            $ \nwId -> do
              info <- showNetwork daemon nwId

              -- Bridge should be running, but no dnsmasq
              nwiRunning info `shouldBe` True
              nwiDnsmasqPid info `shouldBe` Nothing

    describe "Virtual networking" $ do
      it "two VMs can communicate over a virtual network" $ \env -> do
        withTestDaemon env $ \daemon -> do
          -- Create and start a virtual network
          bracket
            (do nwId <- createNetwork daemon "test-bridge"; startNetwork daemon nwId; pure nwId)
            (\nwId -> stopNetwork daemon nwId >> deleteNetwork daemon nwId)
            $ \nwId -> do
              let config = defaultVmConfig {vmcNetworkId = Just nwId, vmcForceTcpSsh = True}
              withTestVmOnDaemon daemon config $ \vm1 -> do
                withTestVmOnDaemon daemon config $ \vm2 -> do
                  -- Configure static IP addresses on the bridge interfaces
                  -- The managed interface is the second NIC (eth1)
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
          -- Create and start a virtual network with DHCP enabled
          bracket
            (do nwId <- createNetworkWithSubnet daemon "test-dhcp" "10.99.0.0/24"; startNetwork daemon nwId; pure nwId)
            (\nwId -> stopNetwork daemon nwId >> deleteNetwork daemon nwId)
            $ \nwId -> do
              let config = defaultVmConfig {vmcNetworkId = Just nwId, vmcForceTcpSsh = True}
              withTestVmOnDaemon daemon config $ \vm1 -> do
                withTestVmOnDaemon daemon config $ \vm2 -> do
                  -- Request DHCP on the managed interface (eth1)
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

          -- Verify the daemon is still running after network stop
          -- (regression test: namespace manager SIGTERM must not propagate to daemon)
          nwCheck <- createNetwork daemon "daemon-alive-check"
          deleteNetwork daemon nwCheck

    describe "NAT" $ do
      it "VM on a NAT-enabled network can reach the internet" $ \env -> do
        withTestDaemon env $ \daemon -> do
          bracket
            (do nwId <- createNetworkWithNat daemon "test-nat" "10.77.0.0/24"; startNetwork daemon nwId; pure nwId)
            (\nwId -> stopNetwork daemon nwId >> deleteNetwork daemon nwId)
            $ \nwId -> do
              let config = defaultVmConfig {vmcNetworkId = Just nwId, vmcForceTcpSsh = True}
              withTestVmOnDaemon daemon config $ \vm -> do
                -- Request DHCP on the managed interface (eth1).
                -- Use -b to background udhcpc so it keeps the lease and routes.
                runInTestVm_ vm "doas sh -c 'ip link set eth1 up && udhcpc -i eth1 -n -b'"

                -- Verify the VM got an address from the subnet
                (_, ipRaw, _) <- runInTestVm vm "ip -4 -o addr show eth1 | awk '{print $4}' | cut -d/ -f1"
                let ip = T.strip ipRaw
                ip `shouldSatisfy` ("10.77.0." `T.isPrefixOf`)

                -- Replace default route to force traffic through the managed
                -- NAT network. SSH still works because QEMU user-mode networking
                -- handles return traffic internally via slirp NAT.
                runInTestVm_ vm "doas ip route replace default via 10.77.0.1 dev eth1"

                -- Verify the VM can reach the internet via the managed interface
                (codePing, _, _) <- runInTestVm vm "ping -c 3 -W 5 8.8.8.8"
                codePing `shouldBe` ExitSuccess

                -- Verify DNS resolution works through NAT
                (codeDns, dnsOut, _) <- runInTestVm vm "nslookup example.com 8.8.8.8"
                codeDns `shouldBe` ExitSuccess
                T.unpack (T.toLower dnsOut) `shouldSatisfy` isInfixOf "example.com"

-- | Wait for a process to exit, polling every 100ms up to n retries.
waitForProcessExit :: Int -> Int -> IO ()
waitForProcessExit _ 0 = pure ()
waitForProcessExit pid retries = do
  alive <- processAlive pid
  when alive $
    threadDelay 100000 >> waitForProcessExit pid (retries - 1)

-- | Check if a process with the given PID is alive (not a zombie).
-- Reads /proc/<pid>/status to check the process state.
processAlive :: Int -> IO Bool
processAlive pid = do
  let statusFile = "/proc/" <> show pid <> "/status"
  exists <- doesFileExist statusFile
  if not exists
    then pure False
    else do
      content <- readFile statusFile
      let stateLines = filter ("State:" `isPrefixOf`) (lines content)
      pure $ case stateLines of
        (s : _) -> not ("zombie" `isInfixOf` s)
        [] -> False

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
