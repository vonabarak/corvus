{-# LANGUAGE OverloadedStrings #-}

module Corvus.DiskCleanupSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (bracket)
import Corvus.Action (runAction)
import Corvus.Handlers.Disk.Cleanup
import Corvus.Handlers.Disk.CleanupGraph
import Corvus.ImageOperationGuard
import Corvus.Images (assignImageTag, publishImage)
import Corvus.Model
import Corvus.Protocol
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import System.Timeout (timeout)
import Test.DSL.When (createTestServerState)
import qualified Test.Database as Db
import Test.Hspec

spec :: Spec
spec = do
  describe "image operation leases" $ do
    it "waits for active operations and permits reentrant cleanup subtasks" $ do
      guard <- newImageOperationGuard
      entered <- newEmptyMVar
      release <- newEmptyMVar
      done <- newEmptyMVar
      _ <- forkIO $ withImageOperationGuard guard False $ putMVar entered () >> takeMVar release
      takeMVar entered
      _ <-
        forkIO $
          withImageOperationGuard guard True $
            withImageOperationGuard guard False (putMVar done ())
      tryTakeMVar done `shouldReturn` Nothing
      putMVar release ()
      timeout 1000000 (takeMVar done) `shouldReturn` Just ()
  describe "cleanup selection and execution" $ do
    it "preserves the actual latest tag, previews without tasks, and prunes unplaced metadata" $
      bracket Db.setupTestDb Db.teardownTestDb $ \env -> do
        state <- createTestServerState (Db.tePool env) (Db.teTempDir env)
        now <- getCurrentTime
        let run :: SqlPersistT IO a -> IO a
            run action = runSqlPool action (Db.tePool env)
        old <- run $ publishImage $ DiskImage "cleanup" FormatQcow2 Nothing now Nothing False
        new <- run $ publishImage $ DiskImage "cleanup" FormatQcow2 Nothing now Nothing False
        run $ assignImageTag old "latest"
        before <- run $ count ([] :: [Filter Task])
        preview <- previewDiskCleanup state (DiskCleanup (Just "cleanup") Nothing False)
        case preview of
          RespDiskCleanup report -> do
            dcrDryRun report `shouldBe` True
            map dcvStatus (dcrVersions report) `shouldBe` ["retained", "planned"]
          _ -> expectationFailure (show preview)
        run (count ([] :: [Filter Task])) `shouldReturn` before
        run (get new) `shouldReturn` Just (DiskImage "cleanup" FormatQcow2 Nothing now Nothing False)
        response <- runAction state "test" (DiskCleanup (Just "cleanup") Nothing False)
        case response of
          RespDiskCleanup report -> dcrRemovedVersions report `shouldBe` 1
          _ -> expectationFailure (show response)
        run (get new) `shouldReturn` Nothing
        remaining <- run (get old)
        remaining `shouldSatisfy` isJust
    it "continues after offline placements and keeps them for retry" $
      bracket Db.setupTestDb Db.teardownTestDb $ \env -> do
        state <- createTestServerState (Db.tePool env) (Db.teTempDir env)
        now <- getCurrentTime
        let run :: SqlPersistT IO a -> IO a
            run action = runSqlPool action (Db.tePool env)
        old <- run $ publishImage $ DiskImage "offline" FormatQcow2 Nothing now Nothing False
        run $ insert_ $ DiskImageNode old (toSqlKey 1) "old.qcow2"
        _ <- run $ publishImage $ DiskImage "offline" FormatQcow2 Nothing now Nothing False
        orphan <- run $ publishImage $ DiskImage "orphan" FormatQcow2 Nothing now Nothing False
        _ <- run $ publishImage $ DiskImage "orphan" FormatQcow2 Nothing now Nothing False
        response <- runAction state "test" (DiskCleanup Nothing Nothing False)
        case response of
          RespDiskCleanup report -> do
            dcrFailures report `shouldBe` 1
            dcrRemovedVersions report `shouldBe` 1
          _ -> expectationFailure (show response)
        run (count [DiskImageNodeDiskImageId ==. old]) `shouldReturn` 1
        run (get orphan) `shouldReturn` Nothing
    it "orders overlays first and applies node-local and final-placement protections" $ do
      now <- getCurrentTime
      let base = toSqlKey 1
          overlay = toSqlKey 2
          alpha = toSqlKey 1
          beta = toSqlKey 2
          graph =
            CleanupGraph
              ( Map.fromList
                  [ (base, DiskImage "base" FormatQcow2 Nothing now Nothing False)
                  , (overlay, DiskImage "overlay" FormatQcow2 Nothing now (Just base) False)
                  ]
              )
              [DiskImageNode base alpha "base", DiskImageNode base beta "base", DiskImageNode overlay beta "overlay"]
              []
              Set.empty
              [(base, beta)]
              Map.empty
      cleanupOrder graph Nothing `shouldBe` [overlay, base]
      placementReason graph base alpha `shouldBe` ""
      placementReason graph base beta `shouldBe` "attached VM on node"
      placementReason (forgetPlacement base beta graph) base alpha `shouldBe` "final placement has VM references"
      let retained = graph {cgAttachments = [], cgTemplates = Set.singleton base}
      versionReason retained True base `shouldBe` "template reference"
      placementReason retained base beta `shouldBe` "backing image required on node"
      fromSqlKey base `shouldBe` 1
