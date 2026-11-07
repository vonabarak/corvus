{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Capnp.Gen.Vm as CGVM
import Corvus.Handlers.VmStatusSink (applyNodeStats, newDaemonVmStatusSink)
import qualified Corvus.Model as M
import qualified Corvus.Node.NodeStats as NS
import Corvus.Node.StatusFreshness (newStatusFreshness)
import Corvus.Types (ServerState)
import qualified Data.Text as T
import Database.Persist (get, getJust, update, (=.))
import Database.Persist.Sql (toSqlKey)
import Supervisors (withSupervisor)
import Test.DSL.Core (getDbPool, getTempDir, runDb)
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
    , CGNA.ramTotal = 0
    , CGNA.ramFree = 0
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
  describe "VM lifecycle ownership" $ do
    testCase "leaves an intentional QEMU exit to the stop operation" $ do
      vmId <- insertVm "stopping-push" VmStopping
      let key = toSqlKey vmId :: M.VmId
      runDb $ update key [M.VmRuntimeGeneration =. Just 7, M.VmSpicePort =. Just 5901]
      observed <- runDb $ get key
      state <- mkState
      case observed of
        Nothing -> liftIO $ expectationFailure "VM disappeared"
        Just vm -> liftIO $ withSupervisor $ \sup -> do
          freshness <- newStatusFreshness (pure 0)
          sink <- C.export @CGNA.VmStatusSink sup (newDaemonVmStatusSink state testNodeId freshness)
          let stats = CGVM.VmStats 0 0 0 100 0 0 0 [] []
              entry =
                CGNA.VmStatusEntry
                  { CGNA.vmId = vmId
                  , CGNA.state = CGNA.VmAgentState'errored
                  , CGNA.qemuPid = 0
                  , CGNA.lastExitCode = 143
                  , CGNA.guestAgentOk = False
                  , CGNA.lastPingMillis = 0
                  , CGNA.netIfs = []
                  , CGNA.stats = stats
                  , CGNA.lifecycleRevision = M.vmLifecycleRevision vm
                  , CGNA.runtimeGeneration = 7
                  , CGNA.guestAgentDeferred = False
                  , CGNA.guestAgentMessage = ""
                  , CGNA.netIfsValid = True
                  , CGNA.startupError = ""
                  }
              snapshot = CGNA.VmStatusSnapshot 0 [entry] (statsWithVersion NS.agentVersion)
          _ <- C.callP #onSnapshot CGNA.VmStatusSink'onSnapshot'params {CGNA.snapshot = snapshot} sink >>= C.waitPipeline
          pure ()
      current <- runDb $ get key
      liftIO $ do
        fmap M.vmStatus current `shouldBe` Just VmStopping
        fmap M.vmSpicePort current `shouldBe` Just (Just 5901)

  describe "deferred and stale guest health" $ do
    testCase "a busy probe neither stamps health nor promotes a loading VM" $ do
      vmId <- insertVm "busy-health" VmLoading
      let key = toSqlKey vmId :: M.VmId
      runDb $ update key [M.VmRuntimeGeneration =. Just 7]
      vm <- runDb $ getJust key
      state <- mkState
      liftIO $ withSupervisor $ \sup -> do
        freshness <- newStatusFreshness (pure 0)
        sink <- C.export @CGNA.VmStatusSink sup (newDaemonVmStatusSink state testNodeId freshness)
        let entry =
              CGNA.VmStatusEntry
                { CGNA.vmId = vmId
                , CGNA.state = CGNA.VmAgentState'running
                , CGNA.qemuPid = 123
                , CGNA.lastExitCode = 0
                , CGNA.guestAgentOk = True
                , CGNA.lastPingMillis = 1000
                , CGNA.netIfs = []
                , CGNA.stats = CGVM.VmStats 0 0 0 100 0 0 0 [] []
                , CGNA.lifecycleRevision = M.vmLifecycleRevision vm
                , CGNA.runtimeGeneration = 7
                , CGNA.guestAgentDeferred = True
                , CGNA.guestAgentMessage = "health probe deferred: connection busy"
                , CGNA.netIfsValid = False
                , CGNA.startupError = ""
                }
            send e =
              C.callP
                #onSnapshot
                CGNA.VmStatusSink'onSnapshot'params
                  { CGNA.snapshot = CGNA.VmStatusSnapshot 0 [e] (statsWithVersion NS.agentVersion)
                  }
                sink
                >>= C.waitPipeline
        _ <- send entry
        pure ()
      current <- runDb $ get key
      liftIO $ do
        fmap M.vmStatus current `shouldBe` Just VmLoading
        fmap M.vmHealthcheck current `shouldBe` Just (M.vmHealthcheck vm)

    testCase "a successful ping from an old generation cannot promote the current VM" $ do
      vmId <- insertVm "stale-health" VmStarting
      let key = toSqlKey vmId :: M.VmId
      runDb $ update key [M.VmRuntimeGeneration =. Just 8]
      vm <- runDb $ getJust key
      state <- mkState
      liftIO $ withSupervisor $ \sup -> do
        freshness <- newStatusFreshness (pure 0)
        sink <- C.export @CGNA.VmStatusSink sup (newDaemonVmStatusSink state testNodeId freshness)
        let entry =
              CGNA.VmStatusEntry
                vmId
                CGNA.VmAgentState'running
                123
                0
                True
                1000
                []
                (CGVM.VmStats 0 0 0 100 0 0 0 [] [])
                (M.vmLifecycleRevision vm)
                7
                ""
                False
                ""
                True
        _ <-
          C.callP
            #onSnapshot
            CGNA.VmStatusSink'onSnapshot'params
              { CGNA.snapshot = CGNA.VmStatusSnapshot 0 [entry] (statsWithVersion NS.agentVersion)
              }
            sink
            >>= C.waitPipeline
        pure ()
      current <- runDb $ get key
      liftIO $ do
        fmap M.vmStatus current `shouldBe` Just VmStarting
        fmap M.vmHealthcheck current `shouldBe` Just (M.vmHealthcheck vm)

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
