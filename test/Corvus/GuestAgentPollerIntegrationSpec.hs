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
import Data.Int (Int64)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), VmConfig (..), defaultVmConfig, withTestVmGuestExecOnDaemon)
import Test.VM.Daemon (TestDaemon, withDaemonConnection, withTestDaemonConfig)
import Test.VM.Rpc (listVmNetIfs)

spec :: Spec
spec = withTestDb $ do
  describe "Guest Agent Poller" $ do
    it "populates healthcheck and guest IP addresses after VM start" $ \env -> do
      -- Use 3s healthcheck interval (fast enough for testing, avoids QGA contention)
      withTestDaemonFastPoll env $ \daemon -> do
        withTestVmGuestExecOnDaemon daemon defaultVmConfig $ \vm -> do
          let vmId = tvmId vm

          -- Poll until healthcheck is set (poller needs time to start and succeed)
          hc1 <- waitForHealthcheck daemon vmId 60

          -- Check net-if list: at least one interface should have guest IPs
          netIfs <- waitForGuestIps daemon vmId 30
          case netIfs of
            (first : _) ->
              niGuestIpAddresses first `shouldSatisfy` maybe False (T.isInfixOf "/")
            [] -> fail "No interfaces with guest IPs found"

          -- Wait for the next poll cycle and verify healthcheck updates
          hc2 <- waitForHealthcheckChange daemon vmId hc1 30
          hc2 `shouldSatisfy` (/= Just hc1)

-- | Poll until healthcheck timestamp is set. Fails after timeout seconds.
waitForHealthcheck :: TestDaemon -> Int64 -> Int -> IO UTCTime
waitForHealthcheck daemon vmId timeoutSec = go timeoutSec
  where
    go 0 = fail $ "Healthcheck not set after " <> show timeoutSec <> "s"
    go n = do
      mHc <- getHealthcheck daemon vmId
      case mHc of
        Just hc -> pure hc
        Nothing -> do
          threadDelay 1000000
          go (n - 1)

-- | Poll until at least one network interface has guest IPs. Fails after timeout seconds.
waitForGuestIps :: TestDaemon -> Int64 -> Int -> IO [NetIfInfo]
waitForGuestIps daemon vmId timeoutSec = go timeoutSec
  where
    go 0 = fail $ "No guest IPs found after " <> show timeoutSec <> "s"
    go n = do
      netIfs <- listVmNetIfs daemon vmId
      let withIps = filter (isJust . niGuestIpAddresses) netIfs
      if null withIps
        then do
          threadDelay 1000000
          go (n - 1)
        else pure withIps

-- | Poll until healthcheck timestamp changes from the given value. Fails after timeout seconds.
waitForHealthcheckChange :: TestDaemon -> Int64 -> UTCTime -> Int -> IO (Maybe UTCTime)
waitForHealthcheckChange daemon vmId oldHc timeoutSec = go timeoutSec
  where
    go 0 = fail $ "Healthcheck did not change after " <> show timeoutSec <> "s"
    go n = do
      mHc <- getHealthcheck daemon vmId
      if mHc /= Just oldHc && isJust mHc
        then pure mHc
        else do
          threadDelay 1000000
          go (n - 1)

-- | Get the healthcheck timestamp from VM details.
getHealthcheck :: TestDaemon -> Int64 -> IO (Maybe UTCTime)
getHealthcheck daemon vmId = do
  res <- withDaemonConnection daemon $ \conn -> showVm conn vmId
  case res of
    Right (Right (Just details)) -> pure $ vdHealthcheck details
    _ -> pure Nothing

-- | Run an action with a daemon configured for fast healthcheck polling (3s interval).
withTestDaemonFastPoll env =
  withTestDaemonConfig env (\cfg -> cfg {qcHealthcheckInterval = 3})
