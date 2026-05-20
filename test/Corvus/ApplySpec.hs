{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Declarative `crv apply` happy + guard paths.
--
-- The handler under test is `Corvus.Handlers.Apply`. `whenApply`
-- (in `Test.DSL.When`) runs both `handleApplyValidate` and
-- `ApplyAction.actionExecute` so the spec exercises the full
-- YAML→DB path. Disks that need a real qemu-img invocation are
-- avoided here — we use `register:` against pre-staged paths,
-- which is pure-DB.
module Corvus.ApplySpec (spec) where

import Corvus.Protocol (ApplyResult (..))
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  ------------------------------------------------------------------
  -- empty config

  describe "whenApply: empty config" $ do
    testCase "succeeds with zero resources created" $ do
      when_ $ whenApply "{}\n"
      then_ $ responseIs $ \case
        RespApplyResult r ->
          null (arSshKeys r)
            && null (arDisks r)
            && null (arNetworks r)
            && null (arVms r)
            && null (arTemplates r)
        _ -> False

  ------------------------------------------------------------------
  -- ssh keys

  describe "whenApply: sshKeys section" $ do
    testCase "creates each listed SSH key" $ do
      let yaml =
            "sshKeys:\n\
            \  - name: alice\n\
            \    publicKey: ssh-ed25519 AAAA-alice\n\
            \  - name: bob\n\
            \    publicKey: ssh-ed25519 AAAA-bob\n"
      when_ $ whenApply yaml
      then_ $ responseIs $ \case
        RespApplyResult r -> length (arSshKeys r) == 2
        _ -> False

  ------------------------------------------------------------------
  -- disks (register path — no qemu-img needed)

  describe "whenApply: register-only disk" $ do
    testCase "creates a disk_image row" $ do
      let yaml =
            "disks:\n\
            \  - name: imported\n\
            \    format: qcow2\n\
            \    register: /baseimages/imported.qcow2\n"
      when_ $ whenApply yaml
      then_ $ do
        responseIs $ \case
          RespApplyResult r -> length (arDisks r) == 1
          _ -> False
        diskImageCount 1
        diskImageExists 1

  ------------------------------------------------------------------
  -- malformed YAML

  describe "whenApply: invalid YAML" $ do
    testCase "surfaces a RespError instead of crashing the handler" $ do
      -- `disks: notalist` confuses the FromJSON instance, which
      -- emits an error message rather than a parsed config.
      when_ $ whenApply "disks: notalist\n"
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

  ------------------------------------------------------------------
  -- ifExists behaviour

  describe "whenApply: ifExists default (error)" $ do
    testCase "refuses to recreate an SSH key with a duplicate name" $ do
      given $ do
        _ <- insertSshKey "shared" "ssh-ed25519 AAAA-shared"
        pure ()
      let yaml =
            "sshKeys:\n\
            \  - name: shared\n\
            \    publicKey: ssh-ed25519 AAAA-shared\n"
      when_ $ whenApply yaml
      then_ $ responseIs $ \case
        -- 'ifExists' defaults to 'error', so the second insert
        -- comes back as RespError; ApplyResult is not returned.
        RespError _ -> True
        _ -> False

  describe "whenApply: ifExists skip" $ do
    testCase "tolerates a duplicate SSH-key when ifExists=skip" $ do
      given $ do
        _ <- insertSshKey "exists" "ssh-ed25519 AAAA-exists"
        pure ()
      let yaml =
            "ifExists: skip\n\
            \sshKeys:\n\
            \  - name: exists\n\
            \    publicKey: ssh-ed25519 AAAA-exists\n"
      when_ $ whenApply yaml
      then_ $ responseIs $ \case
        RespApplyResult _ -> True
        _ -> False
