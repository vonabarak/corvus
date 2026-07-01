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

import Corvus.Action (mkActionContext)
import qualified Corvus.Action as Action
import Corvus.Handlers.Apply (executeApply, handleApplyValidate)
import Corvus.Model
  ( EntityField (..)
  , SshKey (..)
  , Task (..)
  , TaskResult (..)
  , TaskSubsystem (..)
  , Unique (..)
  )
import Corvus.Protocol (ApplyEvent (..), ApplyResult (..))
import Corvus.Schema.Apply (IfExists (..))
import Corvus.Types (ssDbPool)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist (entityVal, getBy, insert)
import Database.Persist.Sql (runSqlPool)
import Test.DSL.Core (runDb)
import Test.DSL.When (withState)
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

  describe "handleApplyValidate: disk checksums" $ do
    testCase "accepts a SHA-256 checksum object on HTTP imports" $ do
      let yaml =
            "disks:\n\
            \  - name: base\n\
            \    import: https://example.com/base.qcow2\n\
            \    format: qcow2\n\
            \    checksum:\n\
            \      algorithm: sha256\n\
            \      value: ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\n"
      result <- withState $ \st -> either pure (const $ pure RespOk) =<< handleApplyValidate st yaml
      liftIO $ result `shouldBe` RespOk

    testCase "rejects a checksum on local imports" $ do
      let yaml =
            "disks:\n\
            \  - name: base\n\
            \    import: /tmp/base.qcow2\n\
            \    format: qcow2\n\
            \    checksum:\n\
            \      algorithm: sha256\n\
            \      value: ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\n"
      result <- withState $ \st -> either pure (const $ pure RespOk) =<< handleApplyValidate st yaml
      liftIO $ case result of
        RespError msg -> msg `shouldSatisfy` T.isInfixOf "HTTP/HTTPS imports"
        other -> expectationFailure $ "expected checksum validation error, got " <> show other

    testCase "rejects checksum values with the wrong hex length" $ do
      let yaml =
            "disks:\n\
            \  - name: base\n\
            \    import: https://example.com/base.qcow2\n\
            \    format: qcow2\n\
            \    checksum:\n\
            \      algorithm: sha512\n\
            \      value: ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\n"
      result <- withState $ \st -> either pure (const $ pure RespOk) =<< handleApplyValidate st yaml
      liftIO $ case result of
        RespError msg -> msg `shouldSatisfy` T.isInfixOf "must be 128 hex characters"
        other -> expectationFailure $ "expected checksum length validation error, got " <> show other

    testCase "rejects unknown checksum algorithms during YAML parsing" $ do
      let yaml =
            "disks:\n\
            \  - name: base\n\
            \    import: https://example.com/base.qcow2\n\
            \    format: qcow2\n\
            \    checksum:\n\
            \      algorithm: sha3\n\
            \      value: abc\n"
      result <- withState $ \st -> either pure (const $ pure RespOk) =<< handleApplyValidate st yaml
      liftIO $ case result of
        RespError msg -> msg `shouldSatisfy` T.isInfixOf "unknown checksum algorithm"
        other -> expectationFailure $ "expected checksum parser error, got " <> show other

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

  ------------------------------------------------------------------
  -- ifExists: overwrite — the apply path replaces an existing
  -- row outright. Verified end-to-end against an SSH key
  -- (the SshKey* helpers are pure-DB) so the test sees the
  -- delete-then-create round-trip without needing qemu-img or a
  -- running VM.

  describe "whenApply: ifExists overwrite (sshKeys)" $ do
    testCase "replaces an existing SSH key's public-key payload" $ do
      given $ do
        _ <- insertSshKey "rotate" "ssh-ed25519 AAAA-old"
        pure ()
      let yaml =
            "ifExists: overwrite\n\
            \sshKeys:\n\
            \  - name: rotate\n\
            \    publicKey: ssh-ed25519 AAAA-new\n"
      when_ $ whenApply yaml
      then_ $ do
        responseIs $ \case
          RespApplyResult r -> length (arSshKeys r) == 1
          _ -> False
        -- Pre-flight passes (no FK from VmSshKey /
        -- TemplateSshKey), the SshKeyDelete subtask runs,
        -- then SshKeyCreate inserts the new public key.
        sshKeyHasPublicKey "rotate" "ssh-ed25519 AAAA-new"

  describe "whenApply: ifExists overwrite refusal (sshKeys)" $ do
    testCase "refuses to overwrite a key that's attached to a VM" $ do
      given $ do
        vmId <- givenVmExists "vm-holding-key"
        keyId <- insertSshKey "shared" "ssh-ed25519 AAAA-shared"
        _ <- attachSshKeyToVm vmId keyId
        pure ()
      let yaml =
            "ifExists: overwrite\n\
            \sshKeys:\n\
            \  - name: shared\n\
            \    publicKey: ssh-ed25519 AAAA-rotated\n"
      when_ $ whenApply yaml
      then_ $ do
        -- The pre-flight returns 'Left' with an actionable
        -- message; the apply surfaces it as a RespError rather
        -- than letting Postgres trip the RESTRICT constraint
        -- on the DELETE.
        responseIs $ \case
          RespError msg ->
            T.isInfixOf "cannot overwrite" msg
              && T.isInfixOf "shared" msg
              && T.isInfixOf "vm-holding-key" msg
          _ -> False
        -- The original key is still present, unchanged.
        sshKeyHasPublicKey "shared" "ssh-ed25519 AAAA-shared"

  ------------------------------------------------------------------
  -- streaming sink: phase + entity events

  describe "executeApply: streaming sink (Phase 13a)" $ do
    testCase "pushes PhaseStart and EntityStart/EntityEnd per entity in order" $ do
      let yaml =
            "sshKeys:\n\
            \  - name: alice\n\
            \    publicKey: ssh-ed25519 AAAA-alice\n\
            \disks:\n\
            \  - name: imported\n\
            \    format: qcow2\n\
            \    register: /baseimages/imported.qcow2\n"
      events <- captureApplyEvents yaml
      let phases = [phase | PhaseStart phase _ <- events]
          starts = [(p, n, k) | EntityStart p n k <- events]
          ends = [(p, n, r) | EntityEnd p n r _ _ <- events]
      liftIO $ do
        phases `shouldBe` ["sshKeys", "disks"]
        starts
          `shouldBe` [ ("sshKeys", "alice", "ssh-key-create")
                     , ("disks", "imported", "disk-register")
                     ]
        map (\(p, n, _) -> (p, n)) ends
          `shouldBe` [ ("sshKeys", "alice")
                     , ("disks", "imported")
                     ]
        all (\(_, _, r) -> T.pack (show r) == "TaskSuccess") ends
          `shouldBe` True

-- | Assert that an SSH key with the given name exists and that
-- its public-key payload matches. Used to verify that
-- 'ifExists: overwrite' actually replaced the row (changing the
-- public key) — vs. silently skipping it.
sshKeyHasPublicKey :: T.Text -> T.Text -> TestM ()
sshKeyHasPublicKey name expected = do
  mEnt <- runDb (getBy (UniqueSshKeyName name))
  case mEnt of
    Nothing -> liftIO $ expectationFailure $ "no SshKey named " <> show name
    Just ent ->
      liftIO $ sshKeyPublicKey (entityVal ent) `shouldBe` expected

-- | Run 'executeApply' against a YAML fixture and collect every
-- 'ApplyEvent' pushed through the sink, in emission order. Used by
-- streaming tests above.
captureApplyEvents :: T.Text -> TestM [ApplyEvent]
captureApplyEvents yaml = do
  eventsRef <- liftIO $ newIORef []
  let sink ev = modifyIORef' eventsRef (ev :)
  resp <- withState $ \st -> do
    validated <- handleApplyValidate st yaml
    case validated of
      Left err -> pure err
      Right cfg -> do
        -- 'executeApply' uses runActionAsSubtask, which inserts a
        -- per-entity row with @parent = acTaskId@. Create a real
        -- parent first so the FK constraint holds.
        now <- getCurrentTime
        parentKey <-
          runSqlPool
            ( insert
                Task
                  { taskParent = Nothing
                  , taskStartedAt = now
                  , taskFinishedAt = Nothing
                  , taskSubsystem = SubApply
                  , taskEntityId = Nothing
                  , taskEntityName = Nothing
                  , taskCommand = "apply"
                  , taskResult = TaskRunning
                  , taskMessage = Nothing
                  , taskClientName = "alice"
                  }
            )
            (ssDbPool st)
        let ctx = (mkActionContext st parentKey "alice") {Action.acApplySink = sink}
        _ <- executeApply ctx cfg IfExistsError
        pure (RespApplyResult emptyResult)
  case resp of
    RespApplyResult _ -> pure ()
    other -> liftIO $ fail $ "executeApply: unexpected response: " <> show other
  liftIO (reverse <$> readIORef eventsRef)
  where
    emptyResult =
      ApplyResult
        { arSshKeys = []
        , arDisks = []
        , arNetworks = []
        , arVms = []
        , arTemplates = []
        }
