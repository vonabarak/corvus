{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the guest agent poller.
-- Verifies that the background polling thread populates healthcheck
-- timestamps and guest IP addresses after a VM starts.
--
-- Run with: stack test --test-arguments="--match GuestAgentPoller"
module Corvus.GuestAgentPollerIntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Corvus.Client (showVm)
import Corvus.Protocol (NetIfInfo (..), VmDetails (..))
import Data.Maybe (isJust)
import qualified Data.Text as T
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), VmConfig (..), defaultVmConfig, withTestVmGuestExec)
import Test.VM.Daemon (withDaemonConnection)
import Test.VM.Rpc (listVmNetIfs)

spec :: Spec
spec = withTestDb $ do
  describe "Guest Agent Poller" $ do
    it "populates healthcheck and guest IP addresses after VM start" $ \env -> do
      withTestVmGuestExec env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            vmId = tvmId vm

        -- The poller should have run by now (withTestVmGuestExec waits for agent).
        -- Give extra time for the poller to complete its first network query.
        threadDelay 5000000

        -- Check vm show: healthcheck should be set
        res <- withDaemonConnection daemon $ \conn -> showVm conn vmId
        case res of
          Right (Right (Just details)) ->
            vdHealthcheck details `shouldSatisfy` isJust
          _ -> fail "Failed to get VM details"

        -- Check net-if list: at least one interface should have guest IPs
        netIfs <- listVmNetIfs daemon vmId
        let withIps = filter (isJust . niGuestIpAddresses) netIfs
        withIps `shouldSatisfy` (not . null)
        -- Verify the IP address format contains CIDR notation
        case withIps of
          (first : _) ->
            niGuestIpAddresses first `shouldSatisfy` maybe False (T.isInfixOf "/")
          [] -> fail "No interfaces with guest IPs found"
