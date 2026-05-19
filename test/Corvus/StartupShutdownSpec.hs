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
              { taskParent = Nothing
              , taskStartedAt = now
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

    testCase "keeps non-stopped VMs untouched on startup" $ do
      -- After Phase 3, corvus-nodeagent owns QEMU + virtiofsd
      -- subprocesses independently of the daemon. A daemon
      -- restart finds the VMs still running, so the DB rows'
      -- status / SPICE port should NOT be reset to error — the
      -- daemon's reconnect re-attaches to the live agent state
      -- and reconciles from there.
      given $ do
        _ <- insertVm "running-vm" VmRunning
        _ <- insertVm "starting-vm" VmStarting
        _ <- insertVm "paused-vm" VmPaused
        _ <- insertVm "stopped-vm" VmStopped
        pure ()

      pool <- getDbPool
      tempDir <- getTempDir
      state <- liftIO $ createTestServerState pool tempDir
      liftIO $ handleStartup state 30

      then_ $ do
        -- Running, Starting, Paused VMs survived; statuses unchanged.
        running <- runDb $ selectList [M.VmStatus ==. VmRunning] []
        starting <- runDb $ selectList [M.VmStatus ==. VmStarting] []
        paused <- runDb $ selectList [M.VmStatus ==. VmPaused] []
        stopped <- runDb $ selectList [M.VmStatus ==. VmStopped] []
        liftIO $ length running `shouldBe` 1
        liftIO $ length starting `shouldBe` 1
        liftIO $ length paused `shouldBe` 1
        liftIO $ length stopped `shouldBe` 1
        -- Nothing was reset to VmError.
        errored <- runDb $ selectList [M.VmStatus ==. VmError] []
        liftIO $ length errored `shouldBe` 0

    testCase "keeps running network rows untouched on startup" $ do
      -- After Phase 4, the agent owns kernel state independently
      -- of the daemon process. A daemon restart finds the bridge /
      -- dnsmasq still alive, so the DB row's NetworkRunning flag
      -- should NOT be reset to False — the daemon's reconnect
      -- async re-applies idempotently from the surviving row.
      given $ do
        nwId <- insertNetwork "live-net" "10.0.0.0/24"
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
            liftIO $ networkRunning nw `shouldBe` True
            liftIO $ networkDnsmasqPid nw `shouldBe` Just 88882
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
              { taskParent = Nothing
              , taskStartedAt = oldTime
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
              { taskParent = Nothing
              , taskStartedAt = now
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

    testCase "keeps running VM rows untouched on shutdown" $ do
      -- corvus-nodeagent owns QEMU + virtiofsd lifecycle.
      -- A daemon graceful shutdown leaves running VMs alone so
      -- they survive the daemon restart; the next 'handleStartup'
      -- reconnects to the agent and finds them still alive.
      given $ do
        _ <- insertVm "running-vm" VmRunning
        _ <- insertVm "starting-vm" VmStarting
        _ <- insertVm "paused-vm" VmPaused
        pure ()

      pool <- getDbPool
      tempDir <- getTempDir
      state <- liftIO $ createTestServerState pool tempDir
      liftIO $ handleGracefulShutdown state

      then_ $ do
        running <- runDb $ selectList [M.VmStatus ==. VmRunning] []
        starting <- runDb $ selectList [M.VmStatus ==. VmStarting] []
        paused <- runDb $ selectList [M.VmStatus ==. VmPaused] []
        stopped <- runDb $ selectList [M.VmStatus ==. VmStopped] []
        liftIO $ length running `shouldBe` 1
        liftIO $ length starting `shouldBe` 1
        liftIO $ length paused `shouldBe` 1
        liftIO $ length stopped `shouldBe` 0

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
