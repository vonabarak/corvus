{-# LANGUAGE OverloadedStrings #-}

-- | Tests for SSH key management.
-- These tests verify the daemon's SSH key handlers work correctly.
module Corvus.SshKeySpec (spec) where

import Corvus.Protocol
import Test.DSL.Given
import Test.DSL.When
import Test.Hspec
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "ssh-key create" $ do
    testCase "creates a new SSH key" $ do
      result <- whenSshKeyCreate "my-key" "ssh-ed25519 AAAA... test"
      thenSshKeyCreated result

    testCase "fails for duplicate name" $ do
      _ <- whenSshKeyCreate "my-key" "ssh-ed25519 AAAA... test1"
      result <- whenSshKeyCreate "my-key" "ssh-ed25519 AAAA... test2"
      thenSshKeyError result "already exists"

  describe "ssh-key delete" $ do
    testCase "deletes an existing SSH key" $ do
      keyId <- givenSshKeyExists "my-key"
      result <- whenSshKeyDelete keyId
      thenSshKeyOk result

    testCase "fails for non-existent SSH key" $ do
      result <- whenSshKeyDelete 999
      thenSshKeyNotFound result

    testCase "fails for SSH key in use" $ do
      vmId <- givenVmExists "test-vm"
      keyId <- givenSshKeyExists "my-key"
      _ <- attachSshKeyToVm vmId keyId
      result <- whenSshKeyDelete keyId
      thenSshKeyInUse result

  describe "ssh-key list" $ do
    testCase "returns empty list when no keys exist" $ do
      result <- whenSshKeyList
      thenSshKeyListIsEmpty result

    testCase "returns all SSH keys" $ do
      _ <- givenSshKeyExists "key1"
      _ <- givenSshKeyExists "key2"
      result <- whenSshKeyList
      thenSshKeyListHasCount result 2

  describe "ssh-key attach" $ do
    testCase "attaches SSH key to VM with cloud-init" $ do
      vmId <- givenCloudInitVmExists "test-vm"
      keyId <- givenSshKeyExists "my-key"
      result <- whenSshKeyAttach vmId keyId
      thenSshKeyOk result

    testCase "fails for VM without cloud-init" $ do
      vmId <- givenVmExists "test-vm"
      keyId <- givenSshKeyExists "my-key"
      result <- whenSshKeyAttach vmId keyId
      thenSshKeyError result "cloud-init is not enabled"

    testCase "fails for non-existent VM" $ do
      keyId <- givenSshKeyExists "my-key"
      result <- whenSshKeyAttach 999 keyId
      thenSshKeyVmNotFound result

    testCase "fails for non-existent SSH key" $ do
      vmId <- givenCloudInitVmExists "test-vm"
      result <- whenSshKeyAttach vmId 999
      thenSshKeyNotFound result

  describe "ssh-key detach" $ do
    testCase "detaches SSH key from VM" $ do
      vmId <- givenCloudInitVmExists "test-vm"
      keyId <- givenSshKeyExists "my-key"
      _ <- attachSshKeyToVm vmId keyId
      result <- whenSshKeyDetach vmId keyId
      thenSshKeyOk result

    testCase "fails for non-existent VM" $ do
      keyId <- givenSshKeyExists "my-key"
      result <- whenSshKeyDetach 999 keyId
      thenSshKeyVmNotFound result

    testCase "fails for SSH key not attached to VM" $ do
      vmId <- givenVmExists "test-vm"
      keyId <- givenSshKeyExists "my-key"
      result <- whenSshKeyDetach vmId keyId
      thenSshKeyNotFound result

  describe "ssh-key list-vm" $ do
    testCase "returns empty list for VM with no SSH keys" $ do
      vmId <- givenVmExists "test-vm"
      result <- whenSshKeyListForVm vmId
      thenSshKeyListIsEmpty result

    testCase "returns all SSH keys for VM" $ do
      vmId <- givenCloudInitVmExists "test-vm"
      keyId1 <- givenSshKeyExists "key1"
      keyId2 <- givenSshKeyExists "key2"
      _ <- attachSshKeyToVm vmId keyId1
      _ <- attachSshKeyToVm vmId keyId2
      result <- whenSshKeyListForVm vmId
      thenSshKeyListHasCount result 2

    testCase "returns not found for non-existent VM" $ do
      result <- whenSshKeyListForVm 999
      thenSshKeyVmNotFound result
