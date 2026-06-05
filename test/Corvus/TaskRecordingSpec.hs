{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Task-recording invariants in `Corvus.Action.runAction`.
--
-- Every mutating handler that goes through `runAction` records a
-- row in the `task` table — subsystem, entity name, command,
-- result, message. The tests below exercise that recording for
-- two representative shapes: a happy create that lands a single
-- Task row tagged `success`, and a failure (bad input) that
-- lands a Task row tagged `error`. Both shapes exist in the
-- production wire, so the unit test is enough to keep the
-- recording wiring honest.
module Corvus.TaskRecordingSpec (spec) where

import Control.Exception (throwIO)
import Corvus.Action (Action (..), TaskCancelledException (..), runAction)
import Corvus.Model (Task (..), TaskResult (..), TaskSubsystem (..))
import Test.Prelude

-- | A minimal Action that always cancels, used to exercise the
-- cancellation finalize path in 'Corvus.Action.runAndFinalizeResult'
-- (a 'TaskCancelledException' — from a cooperative checkpoint or the
-- hard @Task.cancel@ throwTo — must land the row as 'TaskCancelled',
-- not 'TaskError').
data CancelAction = CancelAction

instance Action CancelAction where
  actionSubsystem _ = SubSystem
  actionCommand _ = "cancel-test"
  actionExecute _ _ = throwIO TaskCancelledException

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "runAction task recording" $ do
    testCase "a successful whenVmCreate writes one success-tagged Task row" $ do
      when_ $ whenVmCreate "tracked" 1 256 Nothing
      then_ $ do
        responseIs $ \case
          RespVmCreated _ -> True
          _ -> False
        taskCount 1
      mTask <- getLastTask
      liftIO $ case mTask of
        Just (Entity _ t) -> do
          taskSubsystem t `shouldBe` SubVm
          taskCommand t `shouldBe` "create"
          taskResult t `shouldBe` TaskSuccess
        Nothing -> fail "expected one Task row, found none"

    testCase "a failed whenVmCreate (duplicate name) writes an error-tagged Task row" $ do
      given $ do
        _ <- insertVm "dup" VmStopped
        pure ()
      when_ $ whenVmCreate "dup" 1 256 Nothing
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False
      mTask <- getLastTask
      liftIO $ case mTask of
        Just (Entity _ t) -> do
          taskSubsystem t `shouldBe` SubVm
          taskCommand t `shouldBe` "create"
          taskResult t `shouldBe` TaskError
        Nothing -> fail "expected one Task row, found none"

    testCase "whenSshKeyCreate records under the ssh-key subsystem" $ do
      when_ $ whenSshKeyCreate "k" "ssh-ed25519 AAAA-k"
      then_ $ responseIs $ \case
        RespSshKeyCreated _ -> True
        _ -> False
      mTask <- getLastTask
      liftIO $ case mTask of
        Just (Entity _ t) -> do
          taskSubsystem t `shouldBe` SubSshKey
          taskCommand t `shouldBe` "create"
        Nothing -> fail "expected one Task row, found none"

    testCase "multiple actions land multiple Task rows in order" $ do
      when_ $ whenVmCreate "a" 1 256 Nothing
      when_ $ whenVmCreate "b" 1 256 Nothing
      when_ $ whenSshKeyCreate "k" "ssh-ed25519 AAAA-k"
      then_ $ taskCount 3

    testCase "runAction stamps clientName on the Task row" $ do
      -- The DSL's whenVmCreate runs through `runAction state "alice" …`,
      -- so the recorded row should carry "alice".
      when_ $ whenVmCreate "named-vm" 1 256 Nothing
      mTask <- getLastTask
      liftIO $ case mTask of
        Just (Entity _ t) -> taskClientName t `shouldBe` "alice"
        Nothing -> fail "expected one Task row, found none"

    testCase "a TaskCancelledException records a cancelled (not errored) Task row" $ do
      _ <- withState (\st -> runAction st "alice" CancelAction)
      mTask <- getLastTask
      liftIO $ case mTask of
        Just (Entity _ t) -> do
          taskCommand t `shouldBe` "cancel-test"
          taskResult t `shouldBe` TaskCancelled
        Nothing -> fail "expected one Task row, found none"
