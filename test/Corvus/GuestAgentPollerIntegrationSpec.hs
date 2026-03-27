{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the guest agent poller.
-- Verifies that the background polling thread populates healthcheck
-- timestamps and guest IP addresses after a VM starts, and that the
-- healthcheck timestamp updates periodically.
--
-- Run with: stack test --test-arguments="--match GuestAgentPoller"
module Corvus.GuestAgentPollerIntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Corvus.Client (showVm)
import Corvus.Protocol (NetIfInfo (..), VmDetails (..))
import Corvus.Qemu.Config (qcHealthcheckInterval)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), VmConfig (..), defaultVmConfig, withTestVmGuestExecOnDaemon)
import Test.VM.Daemon (withDaemonConnection, withTestDaemonConfig)
import Test.VM.Rpc (listVmNetIfs)

spec :: Spec
spec = withTestDb $ do
  describe "Guest Agent Poller" $ do
    it "populates healthcheck and guest IP addresses after VM start" $ \env -> do
      -- Use 1s healthcheck interval for faster testing
      withTestDaemonFastPoll env $ \daemon -> do
        withTestVmGuestExecOnDaemon daemon defaultVmConfig $ \vm -> do
          let vmId = tvmId vm

          -- The poller should have run by now (withTestVmGuestExec waits for agent).
          -- Give extra time for the poller to complete its first network query.
          threadDelay 3000000

          -- Read first healthcheck value
          hc1 <- getHealthcheck daemon vmId

          -- Check healthcheck is set
          hc1 `shouldSatisfy` isJust

          -- Check net-if list: at least one interface should have guest IPs
          netIfs <- listVmNetIfs daemon vmId
          let withIps = filter (isJust . niGuestIpAddresses) netIfs
          withIps `shouldSatisfy` (not . null)
          -- Verify the IP address format contains CIDR notation
          case withIps of
            (first : _) ->
              niGuestIpAddresses first `shouldSatisfy` maybe False (T.isInfixOf "/")
            [] -> fail "No interfaces with guest IPs found"

          -- Wait for the next poll cycle and verify healthcheck updates
          threadDelay 3000000
          hc2 <- getHealthcheck daemon vmId
          hc2 `shouldSatisfy` isJust
          hc2 `shouldSatisfy` (/= hc1)

-- | Get the healthcheck timestamp from VM details.
getHealthcheck daemon vmId = do
  res <- withDaemonConnection daemon $ \conn -> showVm conn vmId
  case res of
    Right (Right (Just details)) -> pure $ vdHealthcheck details
    _ -> fail "Failed to get VM details"

-- | Run an action with a daemon configured for fast healthcheck polling (1s interval).
withTestDaemonFastPoll env =
  withTestDaemonConfig env (\cfg -> cfg {qcHealthcheckInterval = 1})
