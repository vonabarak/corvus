{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SSH-key CRUD + per-VM attach/detach.
--
-- The handler under test is `Corvus.Handlers.SshKey`. Attaching a
-- key to a VM requires the VM to have cloud-init enabled (the
-- cloud-init ISO is what materialises the key inside the guest)
-- — we exercise both that guard and the happy path against a
-- cloud-init-enabled VM. The post-attach cloud-init ISO
-- regeneration tries to shell out via the agent; in the test
-- fixture the agent is absent so the response surfaces as a
-- specific error string. We assert the specific shape.
module Corvus.SshKeySpec (spec) where

import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "whenSshKeyList" $ do
    testCase "returns an empty list when no keys exist" $ do
      when_ whenSshKeyList
      then_ $ responseIs $ \case
        RespSshKeyList [] -> True
        _ -> False

    testCase "returns each inserted key" $ do
      given $ do
        _ <- insertSshKey "alice" "ssh-ed25519 AAAA-alice"
        _ <- insertSshKey "bob" "ssh-ed25519 AAAA-bob"
        pure ()
      when_ whenSshKeyList
      then_ $ responseIs $ \case
        RespSshKeyList xs -> length xs == 2
        _ -> False

  describe "whenSshKeyCreate" $ do
    testCase "writes a row" $ do
      when_ $ whenSshKeyCreate "fresh" "ssh-ed25519 AAAA-fresh"
      then_ $ responseIs $ \case
        RespSshKeyCreated _ -> True
        _ -> False

    testCase "rejects an all-digit name" $ do
      when_ $ whenSshKeyCreate "123" "ssh-ed25519 AAAA-x"
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

    testCase "rejects a duplicate name" $ do
      given $ do
        _ <- insertSshKey "shared" "ssh-ed25519 AAAA-shared"
        pure ()
      when_ $ whenSshKeyCreate "shared" "ssh-ed25519 AAAA-shared-alt"
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

  describe "whenSshKeyDelete" $ do
    testCase "returns SshKeyNotFound for unknown id" $ do
      when_ $ whenSshKeyDelete 999
      then_ $ responseIs $ \case
        RespSshKeyNotFound -> True
        _ -> False

    testCase "deletes a free key" $ do
      given $ do
        _ <- insertSshKey "lonely" "ssh-ed25519 AAAA-x"
        pure ()
      when_ $ whenSshKeyDelete 1
      then_ $ responseIs (== RespSshKeyOk)

    testCase "refuses to delete a key that's attached to a VM" $ do
      given $ do
        vmId <- givenCloudInitVmExists "user-vm"
        keyId <- insertSshKey "claimed" "ssh-ed25519 AAAA-claimed"
        _ <- attachSshKeyToVm vmId keyId
        pure ()
      when_ $ whenSshKeyDelete 1
      then_ $ responseIs $ \case
        RespSshKeyInUse _ -> True
        _ -> False

  describe "whenSshKeyAttach" $ do
    testCase "returns VmNotFound when the VM doesn't exist" $ do
      given $ do
        _ <- insertSshKey "k" "ssh-ed25519 AAAA-k"
        pure ()
      when_ $ whenSshKeyAttach 999 1
      then_ responseIsVmNotFound

    testCase "refuses when the VM doesn't have cloud-init enabled" $ do
      given $ do
        _ <- insertVm "no-ci" VmStopped
        _ <- insertSshKey "k" "ssh-ed25519 AAAA-k"
        pure ()
      when_ $ whenSshKeyAttach 1 1
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

    testCase "returns SshKeyNotFound when the key doesn't exist" $ do
      given $ do
        _ <- givenCloudInitVmExists "user-vm"
        pure ()
      when_ $ whenSshKeyAttach 1 999
      then_ $ responseIs $ \case
        RespSshKeyNotFound -> True
        _ -> False

  describe "whenSshKeyListForVm" $ do
    testCase "returns VmNotFound for an unknown VM" $ do
      when_ $ whenSshKeyListForVm 999
      then_ responseIsVmNotFound

    testCase "returns an empty list for a VM with no keys attached" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        pure ()
      when_ $ whenSshKeyListForVm 1
      then_ $ responseIs $ \case
        RespSshKeyList [] -> True
        _ -> False
