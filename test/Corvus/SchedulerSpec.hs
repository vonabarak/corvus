{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the placement scheduler in
-- "Corvus.Handlers.Scheduler".
--
-- 'pickNodeForVm' is the only non-trivial picker — it filters
-- to online nodes, applies the in-memory RAM reservation map on
-- top of the agent-reported @ramMbFree@, scores by (free-RAM,
-- storage GiB, load) and tie-breaks by name. The two "lowest-id
-- online node" helpers ('pickNodeForDisk' / 'pickNodeForNetwork')
-- share one code path and get one case each.
--
-- These tests run against the per-spec test DB (so they share
-- the same Postgres pool the rest of the unit suite uses) but
-- they construct their own 'ServerState' rather than going
-- through the 'whenVmCreate' DSL — the goal is to exercise the
-- scoring + reservation math against synthetic 'Node' rows,
-- which is hard to set up through the public Action surface.
module Corvus.SchedulerSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Corvus.Handlers.Scheduler
  ( pickNodeForDisk
  , pickNodeForNetwork
  , pickNodeForVm
  )
import Corvus.Model (NodeAdminState (..))
import qualified Corvus.Model as M
import Corvus.Types (ServerState (..), reserveRam)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist (deleteWhere, insert)
import Database.Persist.Sql (Filter, fromSqlKey)
import Test.DSL.Core (TestM, getDbPool, getTempDir, runDb)
import Test.DSL.When (createTestServerState)
import Test.Prelude

-- | Wipe the seeded 'test-node' row so each test owns the full
-- node table. The default test fixture seeds id=1 to satisfy
-- foreign keys; the scheduler tests don't need that row and
-- benefit from a clean slate (predictable ids, no surprise
-- "online" entry).
clearNodes :: TestM ()
clearNodes = runDb $ deleteWhere ([] :: [Filter M.Node])

-- | Insert a synthetic 'Node' row. The 'host' / 'nodeAgentPort'
-- pair must be unique across all rows in the same test ('Node'
-- carries a 'UniqueNodeAddress' constraint).
insertNode
  :: Text
  -- ^ name
  -> NodeAdminState
  -> Maybe Int
  -- ^ ramMbFree
  -> Maybe Int
  -- ^ storageBytesFree
  -> Maybe Double
  -- ^ loadAvg1
  -> TestM M.NodeId
insertNode name adminState ramFree storageFree loadAvg = do
  now <- liftIO getCurrentTime
  -- Derive a unique port per name so two synthetic nodes don't
  -- collide on UniqueNodeAddress. Sum of char-codes is enough
  -- entropy for the handful of distinct names the spec inserts.
  let port = 10000 + sum (map fromEnum (T.unpack name)) * 2
  runDb $
    insert
      M.Node
        { M.nodeName = name
        , M.nodeHost = "127.0.0.1"
        , M.nodeNodeAgentPort = port
        , M.nodeNetAgentPort = port + 1
        , M.nodeBasePath = "/tmp"
        , M.nodeDescription = Nothing
        , M.nodeAdminState = adminState
        , M.nodeCreatedAt = now
        , M.nodeCpuCount = Nothing
        , M.nodeRamMbTotal = Nothing
        , M.nodeRamMbFree = ramFree
        , M.nodeStorageBytesTotal = Nothing
        , M.nodeStorageBytesFree = fmap fromIntegral storageFree
        , M.nodeLoadAvg1 = loadAvg
        , M.nodeLoadAvg5 = Nothing
        , M.nodeLoadAvg15 = Nothing
        , M.nodeKernelRelease = Nothing
        , M.nodeAgentVersion = Nothing
        , M.nodeNodeAgentHealthcheck = Nothing
        , M.nodeNetAgentHealthcheck = Nothing
        }

-- | Build a fresh 'ServerState' against the test pool. Skips
-- the 'createTestServerState' stub-node registration step
-- (handled below) so this spec can drive its own node-table
-- shape.
mkState :: TestM ServerState
mkState = do
  pool <- getDbPool
  tmp <- getTempDir
  liftIO $ createTestServerState pool tmp

spec :: Spec
spec = sequential $ withTestDb $ do
  ----------------------------------------------------------------
  -- pickNodeForVm

  describe "pickNodeForVm" $ do
    testCase "errors when no nodes exist" $ do
      clearNodes
      state <- mkState
      r <- liftIO $ pickNodeForVm state 512
      liftIO $ case r of
        Left _ -> pure ()
        Right k -> fail $ "expected Left, got node " <> show (fromSqlKey k)

    testCase "errors when every node is in admin state Draining" $ do
      clearNodes
      _ <- insertNode "drain-a" NodeDraining (Just 16384) Nothing Nothing
      _ <- insertNode "drain-b" NodeMaintenance (Just 16384) Nothing Nothing
      state <- mkState
      r <- liftIO $ pickNodeForVm state 512
      liftIO $ case r of
        Left _ -> pure ()
        Right k -> fail $ "expected Left, got node " <> show (fromSqlKey k)

    testCase "picks the single online node when no stats are reported" $ do
      clearNodes
      k <- insertNode "lonely" NodeOnline Nothing Nothing Nothing
      state <- mkState
      r <- liftIO $ pickNodeForVm state 4096
      liftIO $ r `shouldBe` Right k

    testCase "picks the node with the most free RAM when stats are present" $ do
      clearNodes
      _ <- insertNode "small" NodeOnline (Just 2048) Nothing Nothing
      big <- insertNode "big" NodeOnline (Just 32768) Nothing Nothing
      _ <- insertNode "mid" NodeOnline (Just 8192) Nothing Nothing
      state <- mkState
      r <- liftIO $ pickNodeForVm state 1024
      liftIO $ r `shouldBe` Right big

    testCase "filters out nodes that can't satisfy the RAM request" $ do
      -- safety margin is 512 MiB, so a 4096 MiB request needs
      -- 4608 MiB of effective free; the "tight" node sits at
      -- 4096 and gets filtered out; "ok" wins.
      clearNodes
      _ <- insertNode "tight" NodeOnline (Just 4096) Nothing Nothing
      ok <- insertNode "ok" NodeOnline (Just 8192) Nothing Nothing
      state <- mkState
      r <- liftIO $ pickNodeForVm state 4096
      liftIO $ r `shouldBe` Right ok

    testCase "tie-breaks by node name when scoring is otherwise equal" $ do
      -- Three identically-equipped nodes; alpha wins on name.
      clearNodes
      alpha <- insertNode "alpha" NodeOnline (Just 16384) Nothing Nothing
      _ <- insertNode "bravo" NodeOnline (Just 16384) Nothing Nothing
      _ <- insertNode "charlie" NodeOnline (Just 16384) Nothing Nothing
      state <- mkState
      r <- liftIO $ pickNodeForVm state 1024
      liftIO $ r `shouldBe` Right alpha

    testCase "subtracts the in-memory RAM reservation from free RAM" $ do
      -- Without reservation, "big" wins on raw free RAM; after
      -- reserving 20 GiB against it the effective free drops
      -- below "mid" and "mid" should win.
      clearNodes
      big <- insertNode "big" NodeOnline (Just 32768) Nothing Nothing
      mid <- insertNode "mid" NodeOnline (Just 16384) Nothing Nothing
      state <- mkState
      liftIO $ reserveRam state big 20480
      r <- liftIO $ pickNodeForVm state 1024
      liftIO $ r `shouldBe` Right mid

    testCase "factors free storage into the score when free RAM is tied" $ do
      -- Both nodes have 16 GiB free RAM; "fat" has 100 GiB
      -- storage, "thin" has none. score = ram + gib - 100*load,
      -- so "fat" should win by 100 score points.
      clearNodes
      _ <- insertNode "thin" NodeOnline (Just 16384) Nothing Nothing
      fat <- insertNode "fat" NodeOnline (Just 16384) (Just (100 * 1024 * 1024 * 1024)) Nothing
      state <- mkState
      r <- liftIO $ pickNodeForVm state 1024
      liftIO $ r `shouldBe` Right fat

    testCase "penalises load when free RAM is tied" $ do
      -- Both nodes 16 GiB free; "busy" carries loadAvg1=5
      -- (-500 score points), so "calm" should win.
      clearNodes
      calm <- insertNode "calm" NodeOnline (Just 16384) Nothing Nothing
      _ <- insertNode "busy" NodeOnline (Just 16384) Nothing (Just 5.0)
      state <- mkState
      r <- liftIO $ pickNodeForVm state 1024
      liftIO $ r `shouldBe` Right calm

  ----------------------------------------------------------------
  -- pickNodeForDisk / pickNodeForNetwork

  describe "pickNodeForDisk" $ do
    testCase "errors when no node is online" $ do
      clearNodes
      _ <- insertNode "draining" NodeDraining Nothing Nothing Nothing
      state <- mkState
      r <- liftIO $ pickNodeForDisk state
      liftIO $ case r of
        Left _ -> pure ()
        Right k -> fail $ "expected Left, got node " <> show (fromSqlKey k)

    testCase "picks the lowest-id online node" $ do
      clearNodes
      first' <- insertNode "first" NodeOnline Nothing Nothing Nothing
      _ <- insertNode "second" NodeOnline Nothing Nothing Nothing
      state <- mkState
      r <- liftIO $ pickNodeForDisk state
      liftIO $ r `shouldBe` Right first'

  describe "pickNodeForNetwork" $ do
    testCase "shares the lowest-id-online policy with pickNodeForDisk" $ do
      clearNodes
      first' <- insertNode "first" NodeOnline Nothing Nothing Nothing
      _ <- insertNode "second" NodeOnline Nothing Nothing Nothing
      state <- mkState
      r <- liftIO $ pickNodeForNetwork state
      liftIO $ r `shouldBe` Right first'
