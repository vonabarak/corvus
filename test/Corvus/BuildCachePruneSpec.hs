{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Corvus.Handlers.Build.Cache.pruneCacheTail'.
--
-- Exercises the DB-side invariant: rows whose @stepIndex@ exceeds
-- @k@ are removed (along with their backing 'Snapshot' rows); rows
-- with @stepIndex <= k@ survive. The qemu-img portion of the prune
-- is a best-effort call routed through 'deleteSnapshotViaAgent'
-- which returns 'ImageError' when no node-agent is registered (the
-- default in these tests) — and 'pruneCacheTail' tolerates that,
-- so the DB cleanup still runs.
module Corvus.BuildCachePruneSpec (spec) where

import Control.Monad.Logger (runStdoutLoggingT)
import qualified Corvus.Handlers.Build.Cache as Cache
import qualified Corvus.Model as M
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist (insert, insert_, selectList, (<-.), (==.))
import Database.Persist.Sql (toSqlKey)
import Test.DSL.Core (getDbPool, getTempDir, runDb)
import Test.DSL.When (createTestServerState)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "pruneCacheTail" $ do
    testCase "removes BuildCacheEntry rows with stepIndex > k, keeps the prefix" $ do
      _ <- seedCacheRows "pk-keep-prefix" 5
      runPrune "pk-keep-prefix" 2
      remaining <-
        runDb $
          selectList [M.BuildCacheEntryPipelineKey ==. "pk-keep-prefix"] []
      liftIO $ do
        length remaining `shouldBe` 2
        map (M.buildCacheEntryStepIndex . entityVal) remaining
          `shouldMatchList` [1, 2]

    testCase "removes ALL rows when k = 0" $ do
      _ <- seedCacheRows "pk-purge-all" 4
      runPrune "pk-purge-all" 0
      remaining <-
        runDb $
          selectList [M.BuildCacheEntryPipelineKey ==. "pk-purge-all"] []
      liftIO $ length remaining `shouldBe` 0

    testCase "is a no-op when no rows exceed k" $ do
      _ <- seedCacheRows "pk-noop" 3
      runPrune "pk-noop" 5 -- pipeline has 3 steps; k=5 is past the end
      remaining <-
        runDb $
          selectList [M.BuildCacheEntryPipelineKey ==. "pk-noop"] []
      liftIO $ length remaining `shouldBe` 3

    testCase "leaves other pipelines untouched" $ do
      _ <- seedCacheRows "pk-target" 3
      _ <- seedCacheRows "pk-other" 3
      runPrune "pk-target" 1
      tgt <-
        runDb $
          selectList [M.BuildCacheEntryPipelineKey ==. "pk-target"] []
      other <-
        runDb $
          selectList [M.BuildCacheEntryPipelineKey ==. "pk-other"] []
      liftIO $ do
        length tgt `shouldBe` 1
        length other `shouldBe` 3

    testCase "drops the orphaned Snapshot rows alongside the cache entries" $ do
      snapIds <- seedCacheRows "pk-snap-cleanup" 4
      runPrune "pk-snap-cleanup" 1
      -- Steps 2..4 (3 rows) had their backing Snapshot rows; those
      -- must be gone too. Step 1's Snapshot survives.
      survivors <- runDb $ selectList [M.SnapshotId <-. snapIds] []
      liftIO $ length survivors `shouldBe` 1

    testCase "is idempotent — running twice with the same k is a no-op the second time" $ do
      _ <- seedCacheRows "pk-idem" 5
      runPrune "pk-idem" 2
      after1 <-
        runDb $
          selectList [M.BuildCacheEntryPipelineKey ==. "pk-idem"] []
      runPrune "pk-idem" 2
      after2 <-
        runDb $
          selectList [M.BuildCacheEntryPipelineKey ==. "pk-idem"] []
      liftIO $ do
        length after1 `shouldBe` 2
        length after2 `shouldBe` 2

-- | Insert @n@ cache rows for the given pipeline key under a fresh
-- bake VM + qcow2 disk. Step indices are @1..n@; each step's
-- chainHash is @"<pipelineKey>-c-<i>"@ so rows are distinct on the
-- @UniqueBuildCacheChainDisk@ key. Returns the @SnapshotId@s
-- inserted (one per step, role @artifact@), in step order.
seedCacheRows :: Text -> Int -> TestM [M.SnapshotId]
seedCacheRows pipelineKey n = do
  vmId <- insertVm (pipelineKey <> "-vm") VmStopped
  diskId <-
    insertDiskImageOnTestNode
      (pipelineKey <> "-disk")
      (pipelineKey <> "-disk.qcow2")
      FormatQcow2
  mapM (insertOne vmId diskId) [1 .. n]
  where
    insertOne vmId diskId i = do
      now <- liftIO getCurrentTime
      let chain = pipelineKey <> "-c-" <> T.pack (show i)
      snapId <-
        runDb $
          insert
            M.Snapshot
              { M.snapshotDiskImageId = toSqlKey diskId
              , -- The unique (disk_image_id, name) index forbids
                -- reusing the prefix; use the full chain string to
                -- guarantee distinctness for each i.
                M.snapshotName = "cache-" <> chain
              , M.snapshotCreatedAt = now
              , M.snapshotSizeMb = Nothing
              , M.snapshotLive = True
              , M.snapshotQuiesced = False
              , M.snapshotHasVmstate = False
              }
      runDb $
        insert_
          M.BuildCacheEntry
            { M.buildCacheEntryPipelineKey = pipelineKey
            , M.buildCacheEntryStepIndex = i
            , M.buildCacheEntryChainHash = chain
            , M.buildCacheEntryDiskRole = "artifact"
            , M.buildCacheEntrySnapshotId = snapId
            , M.buildCacheEntryVmId = toSqlKey vmId
            , M.buildCacheEntryCreatedAt = now
            }
      pure snapId

-- | Build a fresh test 'ServerState' and run 'pruneCacheTail'
-- inside the test's logging context. The agent call inside the
-- prune resolves to "nodeagent unavailable" (no node-agent is
-- registered on the seeded test-node); 'pruneCacheTail' logs WARN
-- and keeps going, so the DB cleanup we're asserting still
-- happens.
runPrune :: Text -> Int -> TestM ()
runPrune pipelineKey k = do
  pool <- getDbPool
  tempDir <- getTempDir
  state <- liftIO $ createTestServerState pool tempDir
  liftIO $ runStdoutLoggingT (Cache.pruneCacheTail state pipelineKey k)
