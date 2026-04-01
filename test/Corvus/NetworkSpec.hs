{-# LANGUAGE OverloadedStrings #-}

module Corvus.NetworkSpec (spec) where

import Corvus.Client.Rpc (NetworkResult (..))
import Corvus.Model (NetworkInterface (..))
import Database.Persist (insert)
import Database.Persist.Sql (toSqlKey)
import Test.DSL.Core (runDb)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "network create" $ do
    testCase "creates network with valid subnet" $ do
      result <- whenNetworkCreate "test-net" "10.0.1.0/24"
      liftIO $ case result of
        NetworkCreated nwId -> nwId `shouldSatisfy` (> 0)
        other -> fail $ "Expected NetworkCreated, got: " ++ show other

    testCase "creates network with empty subnet" $ do
      result <- whenNetworkCreate "no-subnet-net" ""
      liftIO $ case result of
        NetworkCreated nwId -> nwId `shouldSatisfy` (> 0)
        other -> fail $ "Expected NetworkCreated, got: " ++ show other

    testCase "fails for duplicate name" $ do
      result1 <- whenNetworkCreate "dup-net" "10.0.0.0/24"
      liftIO $ case result1 of
        NetworkCreated _ -> pure ()
        other -> fail $ "First create should succeed: " ++ show other
      result2 <- whenNetworkCreate "dup-net" "10.0.1.0/24"
      liftIO $ case result2 of
        NetworkError _ -> pure ()
        other -> fail $ "Expected NetworkError for duplicate, got: " ++ show other

    testCase "fails for invalid subnet" $ do
      result <- whenNetworkCreate "bad-net" "not-a-subnet"
      liftIO $ case result of
        NetworkError _ -> pure ()
        other -> fail $ "Expected NetworkError, got: " ++ show other

  describe "network delete" $ do
    testCase "deletes existing network" $ do
      nwId <- do
        result <- whenNetworkCreate "del-net" "10.0.0.0/24"
        liftIO $ case result of
          NetworkCreated nid -> pure nid
          other -> fail $ "Setup failed: " ++ show other
      result <- whenNetworkDelete nwId
      liftIO $ result `shouldBe` NetworkDeleted

    testCase "fails for non-existent network" $ do
      result <- whenNetworkDelete 999
      liftIO $ result `shouldBe` NetworkNotFound

    testCase "fails for network with attached interfaces" $ do
      -- Create network via handler, then create a VM and attach a NIC to the network
      -- We need to use the Request API to set up the network reference on a NIC
      nwResp <- whenNetworkCreate "used-net" "10.0.0.0/24"
      nwId <- liftIO $ case nwResp of
        NetworkCreated nid -> pure nid
        other -> fail $ "Setup network failed: " ++ show other
      -- Create a VM and add a NIC referencing the network (via direct DB insert)
      vmId <- given $ insertVm "test-vm" VmStopped
      _ <- given $ do
        runDb $
          insert
            NetworkInterface
              { networkInterfaceVmId = toSqlKey vmId
              , networkInterfaceInterfaceType = NetVde
              , networkInterfaceHostDevice = ""
              , networkInterfaceMacAddress = "52:54:00:00:00:01"
              , networkInterfaceNetworkId = Just (toSqlKey nwId)
              , networkInterfaceGuestIpAddresses = Nothing
              }
      result <- whenNetworkDelete nwId
      liftIO $ case result of
        NetworkInUse -> pure ()
        other -> fail $ "Expected NetworkInUse, got: " ++ show other

  describe "network list" $ do
    testCase "returns empty list when no networks exist" $ do
      result <- whenNetworkList
      liftIO $ case result of
        NetworkListResult [] -> pure ()
        other -> fail $ "Expected empty list, got: " ++ show other

    testCase "returns all networks" $ do
      _ <- whenNetworkCreate "net1" "10.0.1.0/24"
      _ <- whenNetworkCreate "net2" "10.0.2.0/24"
      result <- whenNetworkList
      liftIO $ case result of
        NetworkListResult nets -> length nets `shouldBe` 2
        other -> fail $ "Expected list, got: " ++ show other

  describe "network show" $ do
    testCase "returns network details" $ do
      nwId <- do
        r <- whenNetworkCreate "show-net" "10.0.3.0/24"
        liftIO $ case r of
          NetworkCreated nid -> pure nid
          other -> fail $ "Setup failed: " ++ show other
      result <- whenNetworkShow nwId
      liftIO $ case result of
        NetworkDetails _ -> pure ()
        other -> fail $ "Expected NetworkDetails, got: " ++ show other

    testCase "returns not found for non-existent network" $ do
      result <- whenNetworkShow 999
      liftIO $ result `shouldBe` NetworkNotFound
