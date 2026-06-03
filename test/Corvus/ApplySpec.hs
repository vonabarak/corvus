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
  , Task (..)
  , TaskResult (..)
  , TaskSubsystem (..)
  )
import Corvus.Protocol (ApplyEvent (..), ApplyResult (..))
import Corvus.Types (ssDbPool)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist (insert)
import Database.Persist.Postgresql (runSqlPool)
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
        _ <- executeApply ctx cfg False
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
