{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the task recording system.
-- Verifies that Actions create task records in the database,
-- including subtask hierarchies and error handling.
module Corvus.TaskRecordingSpec (spec) where

import Corvus.Protocol (Ref (..), Request (..))
import Database.Persist.Sql (fromSqlKey)
import Test.DSL.When (executeRequest)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "task recording" $ do
    describe "basic operations" $ do
      testCase "VM create records a success task" $ do
        _ <- executeRequest (ReqVmCreate "task-test-vm" 1 512 Nothing False False False False)
        then_ $ taskExists SubVm "create" TaskSuccess

      testCase "disk create records a success task" $ do
        _ <- executeRequest (ReqDiskCreate "task-test-disk" FormatQcow2 1024 Nothing)
        then_ $ taskExists SubDisk "create" TaskSuccess

      testCase "SSH key create records a success task" $ do
        _ <- executeRequest (ReqSshKeyCreate "task-test-key" "ssh-ed25519 AAAA test")
        then_ $ taskExists SubSshKey "create" TaskSuccess

      testCase "network create records a success task" $ do
        _ <- executeRequest (ReqNetworkCreate "task-test-net" "" False False False)
        then_ $ taskExists SubNetwork "create" TaskSuccess

    describe "read-only operations" $ do
      testCase "VM list does not record a task" $ do
        when_ vmList
        then_ $ taskCount 0

      testCase "VM show does not record a task" $ do
        when_ $ vmShow 1
        then_ $ taskCount 0

    describe "error handling" $ do
      testCase "duplicate VM create records an error task" $ do
        _ <- executeRequest (ReqVmCreate "dup-vm" 1 512 Nothing False False False False)
        _ <- executeRequest (ReqVmCreate "dup-vm" 1 512 Nothing False False False False)
        then_ $ do
          taskExists SubVm "create" TaskSuccess
          taskExists SubVm "create" TaskError

      testCase "validation failure inside handler records an error task" $ do
        -- Empty name fails validation inside actionExecute (after task record created)
        _ <- executeRequest (ReqVmCreate "" 1 512 Nothing False False False False)
        then_ $ taskExists SubVm "create" TaskError

    describe "task metadata" $ do
      testCase "task records entity name" $ do
        _ <- executeRequest (ReqVmCreate "metadata-vm" 1 512 Nothing False False False False)
        Entity _ t <- getLastTask SubVm "create"
        liftIO $ taskEntityName t `shouldBe` Just "metadata-vm"

      testCase "task records entity ID after creation" $ do
        resp <- executeRequest (ReqVmCreate "id-test-vm" 1 512 Nothing False False False False)
        case resp of
          RespVmCreated vmId -> do
            Entity _ t <- getLastTask SubVm "create"
            liftIO $ taskEntityId t `shouldBe` Just (fromIntegral vmId)
          _ -> liftIO $ fail $ "Expected RespVmCreated, got: " ++ show resp

      testCase "task has start and finish time" $ do
        _ <- executeRequest (ReqVmCreate "time-vm" 1 512 Nothing False False False False)
        Entity _ t <- getLastTask SubVm "create"
        liftIO $ do
          taskFinishedAt t `shouldSatisfy` (/= Nothing)
          taskResult t `shouldBe` TaskSuccess

    describe "subtask hierarchy" $ do
      testCase "VM delete records a task with correct subsystem" $ do
        given $ do
          _ <- insertVm "del-vm" VmStopped
          pure ()
        _ <- executeRequest (ReqVmDelete (Ref "del-vm"))
        then_ $ taskExists SubVm "delete" TaskSuccess
