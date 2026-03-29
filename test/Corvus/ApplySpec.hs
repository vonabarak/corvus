{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Corvus.ApplySpec (spec) where

import Corvus.Protocol (ApplyCreated (..), ApplyResult (..), Response (..))
import Corvus.Utils.Yaml (yaml)
import qualified Data.Text as T
import Test.Prelude

spec :: Spec
spec = sequential $ do
  describe "apply YAML parsing" $ do
    withTestDb $ do
      testCase "rejects invalid YAML" $ do
        when_ $ whenApply "{{invalid yaml"
        then_ $ responseIs $ \case
          RespError _ -> True
          _ -> False

      testCase "applies config with SSH key only" $ do
        when_ $
          whenApply
            [yaml|
              sshKeys:
                - name: test-key
                  publicKey: ssh-ed25519 AAAA testkey
            |]
        then_ $ responseIs $ \case
          RespApplyResult r -> length (arSshKeys r) == 1
          _ -> False

      testCase "applies config with new empty disk" $ do
        when_ $
          whenApply
            [yaml|
              disks:
                - name: test-disk
                  format: qcow2
                  sizeMb: 1024
            |]
        then_ $ responseIs $ \case
          RespApplyResult r -> length (arDisks r) == 1
          _ -> False

      testCase "applies config with VM referencing same-file resources" $ do
        when_ $
          whenApply
            [yaml|
              sshKeys:
                - name: my-key
                  publicKey: ssh-ed25519 AAAA mykey
              disks:
                - name: root-disk
                  format: qcow2
                  sizeMb: 2048
              vms:
                - name: test-vm
                  cpuCount: 1
                  ramMb: 512
                  drives:
                    - disk: root-disk
                      interface: virtio
                  sshKeys:
                    - my-key
            |]
        then_ $ responseIs $ \case
          RespApplyResult r ->
            length (arSshKeys r) == 1
              && length (arDisks r) == 1
              && length (arVms r) == 1
          _ -> False

      testCase "fails on duplicate SSH key names" $ do
        when_ $
          whenApply
            [yaml|
              sshKeys:
                - name: dup-key
                  publicKey: ssh-ed25519 AAAA key1
                - name: dup-key
                  publicKey: ssh-ed25519 AAAA key2
            |]
        then_ $ responseIs $ \case
          RespError msg -> "Duplicate" `T.isInfixOf` msg
          _ -> False

      testCase "fails on VM referencing nonexistent disk" $ do
        when_ $
          whenApply
            [yaml|
              vms:
                - name: bad-vm
                  cpuCount: 1
                  ramMb: 256
                  drives:
                    - disk: nonexistent-disk
                      interface: virtio
            |]
        then_ $ responseIs $ \case
          RespError msg -> "not found" `T.isInfixOf` msg
          _ -> False

      testCase "fails on disk with both import and overlay" $ do
        when_ $
          whenApply
            [yaml|
              disks:
                - name: bad-disk
                  import: /some/path.qcow2
                  overlay: other-disk
            |]
        then_ $ responseIs $ \case
          RespError msg -> "cannot specify more than one" `T.isInfixOf` msg
          _ -> False

      testCase "applies config referencing pre-existing DB disk" $ do
        given $ do
          _ <- insertDiskImage "existing-disk" "existing.qcow2" FormatQcow2
          pure ()
        when_ $
          whenApply
            [yaml|
              vms:
                - name: ref-vm
                  cpuCount: 1
                  ramMb: 256
                  drives:
                    - disk: existing-disk
                      interface: virtio
            |]
        then_ $ responseIs $ \case
          RespApplyResult r -> length (arVms r) == 1
          _ -> False

      testCase "applies config with network and VM" $ do
        when_ $
          whenApply
            [yaml|
              networks:
                - name: test-net
                  subnet: "10.0.0.0/24"
              vms:
                - name: net-vm
                  cpuCount: 1
                  ramMb: 256
                  networkInterfaces:
                    - type: user
            |]
        then_ $ responseIs $ \case
          RespApplyResult r ->
            length (arNetworks r) == 1
              && length (arVms r) == 1
          _ -> False

      testCase "applies empty config" $ do
        when_ $ whenApply "{}"
        then_ $ responseIs $ \case
          RespApplyResult r ->
            null (arSshKeys r)
              && null (arDisks r)
              && null (arNetworks r)
              && null (arVms r)
          _ -> False

      testCase "returns created resource IDs" $ do
        when_ $
          whenApply
            [yaml|
              sshKeys:
                - name: id-test-key
                  publicKey: ssh-ed25519 AAAA idtest
            |]
        then_ $ responseIs $ \case
          RespApplyResult r ->
            case arSshKeys r of
              [c] -> acName c == "id-test-key" && acId c > 0
              _ -> False
          _ -> False
