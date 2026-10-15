{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SSH-key CRUD, assignment, and guest injection validation.
module Corvus.SshKeySpec (spec) where

import Control.Monad (void)
import qualified Corvus.Model as M
import Corvus.Protocol (SshKeyInfo (..))
import Database.Persist (update, (=.))
import Database.Persist.Sql (toSqlKey)
import Test.DSL.Core (runDb)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "whenSshKeyList" $ do
    testCase "returns an empty list when no keys exist" $ do
      _ <- when_ whenSshKeyList
      then_ $ responseIs $ \case
        RespSshKeyList [] -> True
        _ -> False

    testCase "returns each inserted key" $ do
      given $ do
        _ <- insertSshKey "alice" "ssh-ed25519 AAAA-alice"
        _ <- insertSshKey "bob" "ssh-ed25519 AAAA-bob"
        pure ()
      _ <- when_ whenSshKeyList
      then_ $ responseIs $ \case
        RespSshKeyList xs -> length xs == 2
        _ -> False

  describe "whenSshKeyCreate" $ do
    testCase "writes a row" $ do
      _ <- when_ $ whenSshKeyCreate "fresh" "ssh-ed25519 AAAA-fresh"
      then_ $ responseIs $ \case
        RespSshKeyCreated _ -> True
        _ -> False

    testCase "rejects an all-digit name" $ do
      _ <- when_ $ whenSshKeyCreate "123" "ssh-ed25519 AAAA-x"
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

    testCase "rejects a duplicate name" $ do
      given $ do
        _ <- insertSshKey "shared" "ssh-ed25519 AAAA-shared"
        pure ()
      _ <- when_ $ whenSshKeyCreate "shared" "ssh-ed25519 AAAA-shared-alt"
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

  describe "whenSshKeyDelete" $ do
    testCase "returns SshKeyNotFound for unknown id" $ do
      _ <- when_ $ whenSshKeyDelete 999
      then_ $ responseIs $ \case
        RespSshKeyNotFound -> True
        _ -> False

    testCase "deletes a free key" $ do
      given $ do
        _ <- insertSshKey "lonely" "ssh-ed25519 AAAA-x"
        pure ()
      _ <- when_ $ whenSshKeyDelete 1
      then_ $ responseIs (== RespSshKeyOk)

    testCase "refuses to delete a key that's attached to a VM" $ do
      given $ do
        vmId <- givenCloudInitVmExists "user-vm"
        keyId <- insertSshKey "claimed" "ssh-ed25519 AAAA-claimed"
        _ <- attachSshKeyToVm vmId keyId
        pure ()
      _ <- when_ $ whenSshKeyDelete 1
      then_ $ responseIs $ \case
        RespSshKeyInUse _ -> True
        _ -> False

  describe "whenSshKeyAttach" $ do
    testCase "returns VmNotFound when the VM doesn't exist" $ do
      given $ do
        _ <- insertSshKey "k" "ssh-ed25519 AAAA-k"
        pure ()
      _ <- when_ $ whenSshKeyAttach 999 1
      then_ responseIsVmNotFound

    testCase "attaches without cloud-init and lists the assignment" $ do
      given $ do
        _ <- insertVm "no-ci" VmStopped
        _ <- insertSshKey "k" "ssh-ed25519 AAAA-k"
        pure ()
      _ <- when_ $ whenSshKeyAttach 1 1
      then_ $ responseIs (== RespSshKeyOk)
      _ <- when_ $ whenSshKeyListForVm 1
      then_ $ responseIs $ \case
        RespSshKeyList [key] -> skiPublicKey key == "ssh-ed25519 AAAA-k"
        _ -> False

    testCase "returns SshKeyNotFound when the key doesn't exist" $ do
      given $ do
        _ <- givenCloudInitVmExists "user-vm"
        pure ()
      _ <- when_ $ whenSshKeyAttach 1 999
      then_ $ responseIs $ \case
        RespSshKeyNotFound -> True
        _ -> False

  describe "whenSshKeyListForVm" $ do
    testCase "returns VmNotFound for an unknown VM" $ do
      _ <- when_ $ whenSshKeyListForVm 999
      then_ responseIsVmNotFound

    testCase "returns an empty list for a VM with no keys attached" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        pure ()
      _ <- when_ $ whenSshKeyListForVm 1
      then_ $ responseIs $ \case
        RespSshKeyList [] -> True
        _ -> False

  describe "whenSshKeyInject" $ do
    testCase "rejects missing VMs" $ do
      _ <- when_ $ whenSshKeyInject 999 "root" False
      then_ responseIsVmNotFound
    testCase "requires a running VM" $ do
      given $ void $ insertVm "stopped" VmStopped
      _ <- when_ $ whenSshKeyInject 1 "root" False
      then_ $ responseIs (== RespVmNotRunning)
    testCase "reports routing failure for a valid assigned-key request" $ do
      given $ do
        vmId <- insertRunningVmWithGuestAgent "running-agent"
        keyId <- insertSshKey "admin" "ssh-ed25519 AAAA-admin"
        _ <- attachSshKeyToVm vmId keyId
        pure ()
      _ <- when_ $ whenSshKeyInject 1 "root" True
      then_ $ responseIs $ \case
        RespGuestAgentError _ -> True
        _ -> False
    testCase "requires an enabled guest agent" $ do
      given $ void $ insertVm "running" VmRunning
      _ <- when_ $ whenSshKeyInject 1 "root" False
      then_ $ responseIs (== RespGuestAgentNotEnabled)
    mapM_
      ( \reset -> testCase "rejects empty assignments even with reset" $ do
          given $ do
            vmId <- insertVm "running-agent" VmRunning
            runDb $ update (toSqlKey vmId :: M.VmId) [M.VmGuestAgent =. True]
          _ <- when_ $ whenSshKeyInject 1 "root" reset
          then_ $ responseIs (== RespError "VM has no assigned SSH keys")
      )
      [False, True]
    mapM_
      ( \username -> testCase "rejects invalid guest usernames" $ do
          _ <- when_ $ whenSshKeyInject 1 username True
          then_ $ responseIs $ \case
            RespError "Guest username must be nonempty and contain no NUL" -> True
            _ -> False
      )
      ["", "root\0bad"]
