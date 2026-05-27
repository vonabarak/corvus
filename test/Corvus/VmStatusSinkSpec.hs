{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unit coverage for the 'applyNodeStats' auto-drain gate.
--
-- A nodeagent reports its baked git-short-hash as
-- 'NodeStats.agentVersion'. The daemon compares it to its own
-- 'NS.agentVersion'; on mismatch the node is flipped to
-- 'NodeDraining' so the scheduler skips it but live state
-- survives. The historical implementation re-fired that flip on
-- every push (~10 s), so an operator's manual
-- @crv node edit --admin-state online@ silently reverted on the
-- next snapshot.
--
-- The fix in 'applyNodeStats' captures the previously-stored
-- agent version BEFORE the update and only calls
-- 'refuseMismatchedAgent' on a transition — i.e. when the new
-- snapshot's version differs from the one already on the row.
-- That way a manual flip-back is sticky against repeated
-- pushes from the same agent build, but a genuine agent rebuild
-- (different hash, still mismatched) still triggers a fresh
-- auto-drain so operators don't miss the warning.
module Corvus.VmStatusSinkSpec (spec) where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import Control.Monad.IO.Class (liftIO)
import Corvus.Handlers.VmStatusSink (applyNodeStats)
import qualified Corvus.Model as M
import qualified Corvus.Node.NodeStats as NS
import Corvus.Types (ServerState)
import Data.Int (Int64)
import qualified Data.Text as T
import Database.Persist (get, update, (=.))
import Database.Persist.Sql (toSqlKey)
import Test.DSL.Core (TestM, getDbPool, getTempDir, runDb)
import Test.DSL.When (createTestServerState)
import Test.Prelude

-- | Minimal 'NodeStats' carrying only an 'agentVersion'. Every
-- other field is left at its numeric/text default; the daemon's
-- 'applyNodeStats' treats those as "no observation" via the
-- 'maybeIfNonZero' / 'maybeText' helpers, so the resulting DB
-- write touches only the agent-version + healthcheck columns.
statsWithVersion :: T.Text -> C.Parsed CGNA.NodeStats
statsWithVersion ver =
  CGNA.NodeStats
    { CGNA.cpuCount = 0
    , CGNA.ramMbTotal = 0
    , CGNA.ramMbFree = 0
    , CGNA.storageBytesTotal = 0
    , CGNA.storageBytesFree = 0
    , CGNA.loadAvg1 = 0
    , CGNA.loadAvg5 = 0
    , CGNA.loadAvg15 = 0
    , CGNA.kernelRelease = T.empty
    , CGNA.agentVersion = ver
    }

-- | The default-seeded test node lives at id=1 (see
-- 'Test.Database.insertDefaultTestNode').
testNodeId :: M.NodeId
testNodeId = toSqlKey 1

mkState :: TestM ServerState
mkState = do
  pool <- getDbPool
  tmp <- getTempDir
  liftIO $ createTestServerState pool tmp

-- | Read the seeded node's admin state + stored agent version.
readNodeState :: TestM (M.NodeAdminState, Maybe T.Text)
readNodeState = do
  mNode <- runDb $ get testNodeId
  case mNode of
    Nothing -> liftIO $ fail "seeded test node disappeared"
    Just n -> pure (M.nodeAdminState n, M.nodeAgentVersion n)

setNodeOnline :: TestM ()
setNodeOnline =
  runDb $ update testNodeId [M.NodeAdminState =. M.NodeOnline]

callApply :: ServerState -> T.Text -> Int64 -> TestM ()
callApply state ver tick =
  liftIO $ applyNodeStats state testNodeId (statsWithVersion ver) tick

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "applyNodeStats version-mismatch auto-drain" $ do
    testCase "first mismatched push flips an online node to draining" $ do
      state <- mkState
      let badVer = "wrong-hash"
      callApply state badVer 1000
      (adminState, storedVer) <- readNodeState
      liftIO $ adminState `shouldBe` M.NodeDraining
      liftIO $ storedVer `shouldBe` Just badVer

    testCase
      "subsequent push from same agent build is sticky after operator flips online"
      $ do
        state <- mkState
        let badVer = "wrong-hash"
        -- 1. First push triggers auto-drain.
        callApply state badVer 1000
        (admin1, _) <- readNodeState
        liftIO $ admin1 `shouldBe` M.NodeDraining
        -- 2. Operator flips back to online (simulates
        --    'crv node edit --admin-state online').
        setNodeOnline
        -- 3. Next push from the same agent build must NOT
        --    re-drain — this is the Fix 1 invariant.
        callApply state badVer 2000
        (admin2, _) <- readNodeState
        liftIO $ admin2 `shouldBe` M.NodeOnline

    testCase
      "a genuinely new mismatch (agent rebuilt to different hash) re-fires auto-drain"
      $ do
        state <- mkState
        callApply state "wrong-hash-v1" 1000
        setNodeOnline
        -- Different mismatched hash → fresh transition → fire.
        callApply state "wrong-hash-v2" 2000
        (adminState, storedVer) <- readNodeState
        liftIO $ adminState `shouldBe` M.NodeDraining
        liftIO $ storedVer `shouldBe` Just "wrong-hash-v2"

    testCase "a matching agent version never drains" $ do
      state <- mkState
      callApply state NS.agentVersion 1000
      (adminState, storedVer) <- readNodeState
      liftIO $ adminState `shouldBe` M.NodeOnline
      liftIO $ storedVer `shouldBe` Just NS.agentVersion
