{-# LANGUAGE OverloadedStrings #-}

-- | Tests for network interface management.
-- These tests verify the daemon's network interface handlers work correctly.
module Corvus.NetIfSpec (spec) where

import Corvus.Model (NetInterfaceType (..))
import Corvus.Protocol
import Test.DSL.Given
import Test.DSL.When
import Test.Hspec
import Test.Prelude

spec :: Spec
spec = withTestDb $ do
  describe "net-if list" $ do
    testCase "returns empty list for VM with no network interfaces" $ do
      vmId <- givenVmExists "test-vm"
      result <- whenNetIfList vmId
      thenNetIfListIsEmpty result

    testCase "returns all network interfaces for VM" $ do
      vmId <- givenVmExists "test-vm"
      _ <- insertNetworkInterface vmId NetBridge "br0" "52:54:00:00:00:01"
      _ <- insertNetworkInterface vmId NetTap "tap0" "52:54:00:00:00:02"
      result <- whenNetIfList vmId
      thenNetIfListHasCount result 2

    testCase "returns not found for non-existent VM" $ do
      result <- whenNetIfList 999
      thenNetIfVmNotFound result

  describe "net-if add" $ do
    testCase "adds network interface to stopped VM" $ do
      vmId <- givenVmExists "test-vm"
      result <- whenNetIfAdd vmId NetBridge "br0" "52:54:00:00:00:01"
      thenNetIfAdded result

    testCase "fails for non-existent VM" $ do
      result <- whenNetIfAdd 999 NetBridge "br0" "52:54:00:00:00:01"
      thenNetIfVmNotFound result

  describe "net-if remove" $ do
    testCase "removes network interface from stopped VM" $ do
      vmId <- givenVmExists "test-vm"
      netIfId <- insertNetworkInterface vmId NetBridge "br0" "52:54:00:00:00:01"
      result <- whenNetIfRemove vmId netIfId
      thenNetIfOk result

    testCase "fails for non-existent VM" $ do
      result <- whenNetIfRemove 999 1
      thenNetIfVmNotFound result

    testCase "fails for non-existent network interface" $ do
      vmId <- givenVmExists "test-vm"
      result <- whenNetIfRemove vmId 999
      thenNetIfNotFound result
