{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for daemon startup and graceful shutdown handlers.
module Corvus.StartupShutdownSpec (spec) where

import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol (Response (..))
import Corvus.Server (handleGracefulShutdown, handleStartup)
import Data.Int (Int64)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Test.DSL.Core (TestM, getDbPool, getTempDir, runDb)
import Test.DSL.When (createTestServerState)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "handleStartup" $ do
    testCase "marks stale running tasks as error" $ do
      -- Insert a task that was "running" when the daemon crashed
      given $ do
        now <- liftIO getCurrentTime
        runDb $
          insert
            Task
              { taskStartedAt = now
              , taskFinishedAt = Nothing
              , taskSubsystem = SubVm
              , taskEntityId = Just 1
              , taskEntityName = Just "stale-vm"
              , taskCommand = "start"
              , taskResult = TaskRunning
              , taskMessage = Nothing
              }
        pure ()

      -- Run startup
      pool <- getDbPool
      tempDir <- getTempDir
      state <- liftIO $ createTestServerState pool tempDir
      liftIO $ handleStartup state 30

      -- Verify the stale task was marked as error
      then_ $ do
        tasks <- runDb $ selectList [TaskCommand ==. "start", TaskResult ==. TaskError] []
        liftIO $ length tasks `shouldSatisfy` (>= 1)
        case tasks of
          (Entity _ t : _) -> liftIO $ taskMessage t `shouldBe` Just "Daemon restarted"
          _ -> pure ()

    testCase "resets stale VMs to error state" $ do
      given $ do
        -- Insert VMs in various non-stopped states with fake PIDs
        vmId1 <- insertVm "running-vm" VmRunning
        vmId2 <- insertVm "starting-vm" VmStarting
        vmId3 <- insertVm "paused-vm" VmPaused
        _ <- insertVm "stopped-vm" VmStopped
        -- Set fake PIDs (processes don't actually exist)
        runDb $ update (toSqlKey vmId1 :: VmId) [M.VmPid =. Just 99991]
        runDb $ update (toSqlKey vmId2 :: VmId) [M.VmPid =. Just 99992]
        runDb $ update (toSqlKey vmId3 :: VmId) [M.VmPid =. Just 99993]
        pure ()

      pool <- getDbPool
      tempDir <- getTempDir
      state <- liftIO $ createTestServerState pool tempDir
      liftIO $ handleStartup state 30

      then_ $ do
        -- Running, Starting, Paused VMs should be Error with no PID
        vms <- runDb $ selectList [M.VmStatus ==. VmError] []
        liftIO $ length vms `shouldBe` 3
        liftIO $ all (\(Entity _ v) -> isNothing (vmPid v)) vms `shouldBe` True

        -- Stopped VM should remain stopped
        stoppedVms <- runDb $ selectList [M.VmStatus ==. VmStopped] []
        liftIO $ length stoppedVms `shouldBe` 1

    testCase "clears stale network state" $ do
      given $ do
        nwId <- insertNetwork "stale-net" "10.0.0.0/24"
        -- Set fake running state (processes don't actually exist)
        runDb $ update (toSqlKey nwId :: NetworkId) [M.NetworkRunning =. True, M.NetworkDnsmasqPid =. Just 88882]
        pure ()

      pool <- getDbPool
      tempDir <- getTempDir
      state <- liftIO $ createTestServerState pool tempDir
      liftIO $ handleStartup state 30

      then_ $ do
        networks <- runDb $ selectList ([] :: [Filter Network]) []
        case networks of
          (Entity _ nw : _) -> do
            liftIO $ networkRunning nw `shouldBe` False
            liftIO $ networkDnsmasqPid nw `shouldBe` Nothing
          _ -> liftIO $ fail "No networks found"

    testCase "records startup task as success" $ do
      pool <- getDbPool
      tempDir <- getTempDir
      state <- liftIO $ createTestServerState pool tempDir
      liftIO $ handleStartup state 30

      then_ $ do
        tasks <- runDb $ selectList [TaskCommand ==. "startup", TaskResult ==. TaskSuccess] []
        liftIO $ length tasks `shouldBe` 1
        case tasks of
          (Entity _ t : _) -> do
            liftIO $ taskSubsystem t `shouldBe` SubSystem
            liftIO $ isJust (taskFinishedAt t) `shouldBe` True
          _ -> pure ()

    testCase "deletes old task entries" $ do
      given $ do
        now <- liftIO getCurrentTime
        let oldTime = addUTCTime (-(100 * 86400)) now -- 100 days ago
        runDb $
          insert
            Task
              { taskStartedAt = oldTime
              , taskFinishedAt = Just oldTime
              , taskSubsystem = SubVm
              , taskEntityId = Nothing
              , taskEntityName = Nothing
              , taskCommand = "old-task"
              , taskResult = TaskSuccess
              , taskMessage = Nothing
              }
        pure ()

      pool <- getDbPool
      tempDir <- getTempDir
      state <- liftIO $ createTestServerState pool tempDir
      liftIO $ handleStartup state 30 -- 30 day retention
      then_ $ do
        oldTasks <- runDb $ selectList [TaskCommand ==. "old-task"] []
        liftIO $ length oldTasks `shouldBe` 0

  describe "handleGracefulShutdown" $ do
    testCase "marks running tasks as error" $ do
      given $ do
        now <- liftIO getCurrentTime
        runDb $
          insert
            Task
              { taskStartedAt = now
              , taskFinishedAt = Nothing
              , taskSubsystem = SubDisk
              , taskEntityId = Nothing
              , taskEntityName = Nothing
              , taskCommand = "import"
              , taskResult = TaskRunning
              , taskMessage = Nothing
              }
        pure ()

      pool <- getDbPool
      tempDir <- getTempDir
      state <- liftIO $ createTestServerState pool tempDir
      liftIO $ handleGracefulShutdown state

      then_ $ do
        tasks <- runDb $ selectList [TaskCommand ==. "import", TaskResult ==. TaskError] []
        liftIO $ length tasks `shouldBe` 1
        case tasks of
          (Entity _ t : _) -> liftIO $ taskMessage t `shouldBe` Just "Daemon shutting down"
          _ -> pure ()

    testCase "resets running VMs to stopped" $ do
      given $ do
        vmId1 <- insertVm "running-vm" VmRunning
        vmId2 <- insertVm "starting-vm" VmStarting
        vmId3 <- insertVm "paused-vm" VmPaused
        -- Set fake PIDs
        runDb $ update (toSqlKey vmId1 :: VmId) [M.VmPid =. Just 77771]
        runDb $ update (toSqlKey vmId2 :: VmId) [M.VmPid =. Just 77772]
        runDb $ update (toSqlKey vmId3 :: VmId) [M.VmPid =. Just 77773]
        pure ()

      pool <- getDbPool
      tempDir <- getTempDir
      state <- liftIO $ createTestServerState pool tempDir
      liftIO $ handleGracefulShutdown state

      then_ $ do
        -- All VMs should be stopped with no PID
        stoppedVms <- runDb $ selectList [M.VmStatus ==. VmStopped] []
        liftIO $ length stoppedVms `shouldBe` 3
        liftIO $ all (\(Entity _ v) -> isNothing (vmPid v)) stoppedVms `shouldBe` True

    testCase "records shutdown task as success" $ do
      pool <- getDbPool
      tempDir <- getTempDir
      state <- liftIO $ createTestServerState pool tempDir
      liftIO $ handleGracefulShutdown state

      then_ $ do
        tasks <- runDb $ selectList [TaskCommand ==. "shutdown", TaskResult ==. TaskSuccess] []
        liftIO $ length tasks `shouldBe` 1
        case tasks of
          (Entity _ t : _) -> do
            liftIO $ taskSubsystem t `shouldBe` SubSystem
            liftIO $ isJust (taskFinishedAt t) `shouldBe` True
            liftIO $ taskMessage t `shouldBe` Just "Shutdown complete"
          _ -> pure ()
