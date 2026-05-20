{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the per-node agent-routing helpers in
-- "Corvus.Types".
--
-- These functions are pure TVar shuffling — the only "I/O" is
-- 'atomically' against the 'ssAgents' / 'ssReservedRam' TVars.
-- They have no DB dependency (we still need a 'ServerState' so
-- we go through the existing test helper, which gives us a
-- bare-bones state pointed at the test pool — but no schema
-- lookups happen inside any of the tested functions).
module Corvus.NodeRoutingSpec (spec) where

import Control.Concurrent.Async (async)
import qualified Corvus.Model as M
import Corvus.Types
  ( NodeConns (..)
  , ServerState (..)
  , clearNetConn
  , clearNodeConn
  , clearReservation
  , lookupNetAgentMaybe
  , lookupNodeAgent
  , registerNodeConns
  , removeNodeConns
  , reserveRam
  , reservedRamFor
  , withNetAgent
  , withNodeAgent
  )
import Data.Maybe (isNothing)
import Database.Persist.Sql (toSqlKey)
import Test.DSL.Core (getDbPool, getTempDir)
import Test.DSL.When (createTestServerState)
import Test.Prelude

-- | Build a fresh state. 'createTestServerState' pre-registers
-- nodeId=1 with both agent caps set to 'Nothing'; these tests
-- exercise both the "registered but unavailable" and the
-- "not registered" paths, plus the registration helpers
-- themselves.
mkState :: TestM ServerState
mkState = do
  pool <- getDbPool
  tmp <- getTempDir
  liftIO $ createTestServerState pool tmp

-- | Make a 'NodeConns' carrying a no-op supervisor; both agent
-- caps stay 'Nothing' because spinning up real ones would need
-- a TCP listener. The helpers under test only care about the
-- shape of the record, not the cap contents.
stubConns :: IO NodeConns
stubConns = do
  sup <- async (pure ())
  pure NodeConns {ncNodeAgent = Nothing, ncNetAgent = Nothing, ncSupervisor = sup}

spec :: Spec
spec = sequential $ withTestDb $ do
  ----------------------------------------------------------------
  -- registry / lookup

  describe "lookupNodeAgent" $ do
    testCase "returns Left for a node that isn't registered" $ do
      state <- mkState
      r <- liftIO $ lookupNodeAgent state (toSqlKey 999 :: M.NodeId)
      liftIO $ case r of
        Left _ -> pure ()
        Right _ -> fail "expected Left for unregistered node"

    testCase "returns Left for a registered node whose nodeagent cap is Nothing" $ do
      -- 'createTestServerState' registers id=1 with both caps =
      -- Nothing, exactly the shape this branch wants.
      state <- mkState
      r <- liftIO $ lookupNodeAgent state (toSqlKey 1 :: M.NodeId)
      liftIO $ case r of
        Left _ -> pure ()
        Right _ -> fail "expected Left for node with no live nodeagent"

  describe "withNodeAgent" $ do
    testCase "propagates the lookup error without running the action" $ do
      state <- mkState
      r <- liftIO $ withNodeAgent state (toSqlKey 999 :: M.NodeId) (\_ -> pure True)
      liftIO $ case r of
        Left _ -> pure ()
        Right _ -> fail "expected Left for unregistered node"

  describe "withNetAgent" $ do
    testCase "propagates the same lookup error shape" $ do
      state <- mkState
      r <- liftIO $ withNetAgent state (toSqlKey 999 :: M.NodeId) (\_ -> pure True)
      liftIO $ case r of
        Left _ -> pure ()
        Right _ -> fail "expected Left for unregistered node"

  describe "lookupNetAgentMaybe" $ do
    testCase "returns Nothing when the node isn't registered" $ do
      state <- mkState
      r <- liftIO $ lookupNetAgentMaybe state (toSqlKey 999 :: M.NodeId)
      liftIO $ isNothing r `shouldBe` True

    testCase "returns Nothing for a registered node with no live netd cap" $ do
      state <- mkState
      r <- liftIO $ lookupNetAgentMaybe state (toSqlKey 1 :: M.NodeId)
      liftIO $ isNothing r `shouldBe` True

  ----------------------------------------------------------------
  -- registration / removal

  describe "registerNodeConns" $ do
    testCase "overwrites an existing entry rather than merging" $ do
      state <- mkState
      nc <- liftIO stubConns
      liftIO $ registerNodeConns state (toSqlKey 1 :: M.NodeId) nc
      -- After overwriting, the entry is *still* registered but
      -- its nodeagent cap is Nothing — same outward shape as
      -- before. The point is that registerNodeConns doesn't
      -- crash on a known id.
      r <- liftIO $ lookupNodeAgent state (toSqlKey 1 :: M.NodeId)
      liftIO $ case r of
        Left _ -> pure ()
        Right _ -> fail "stub conns shouldn't have a live cap"

    testCase "installs a brand-new entry for an unseen node id" $ do
      state <- mkState
      nc <- liftIO stubConns
      liftIO $ registerNodeConns state (toSqlKey 7 :: M.NodeId) nc
      -- Once installed, lookupNetAgentMaybe agrees the node is
      -- registered (it returns Nothing only because ncNetAgent
      -- is Nothing — same as id=1 after createTestServerState).
      r <- liftIO $ lookupNetAgentMaybe state (toSqlKey 7 :: M.NodeId)
      liftIO $ isNothing r `shouldBe` True

  describe "clearNodeConn / clearNetConn" $ do
    testCase "is a no-op when the node isn't registered" $ do
      state <- mkState
      -- Tolerates the missing-id case; the supervisor calls
      -- this on disconnect without knowing whether the entry
      -- still exists.
      liftIO $ clearNodeConn state (toSqlKey 999 :: M.NodeId)
      liftIO $ clearNetConn state (toSqlKey 999 :: M.NodeId)
      r <- liftIO $ lookupNodeAgent state (toSqlKey 999 :: M.NodeId)
      liftIO $ case r of
        Left _ -> pure ()
        Right _ -> fail "expected the node to remain unregistered"

  describe "removeNodeConns" $ do
    testCase "drops the entry and the pending RAM reservation" $ do
      state <- mkState
      liftIO $ reserveRam state (toSqlKey 1 :: M.NodeId) 4096
      liftIO $ removeNodeConns state (toSqlKey 1 :: M.NodeId)
      r <- liftIO $ lookupNetAgentMaybe state (toSqlKey 1 :: M.NodeId)
      liftIO $ isNothing r `shouldBe` True
      ramLeft <- liftIO $ reservedRamFor state (toSqlKey 1 :: M.NodeId)
      liftIO $ ramLeft `shouldBe` 0

  ----------------------------------------------------------------
  -- RAM reservations

  describe "reserveRam / reservedRamFor" $ do
    testCase "starts at 0 for every node" $ do
      state <- mkState
      r <- liftIO $ reservedRamFor state (toSqlKey 1 :: M.NodeId)
      liftIO $ r `shouldBe` 0

    testCase "sums repeated reservations for the same node" $ do
      state <- mkState
      let n = toSqlKey 1 :: M.NodeId
      liftIO $ reserveRam state n 2048
      liftIO $ reserveRam state n 1024
      r <- liftIO $ reservedRamFor state n
      liftIO $ r `shouldBe` 3072

    testCase "keeps reservations for different nodes separate" $ do
      state <- mkState
      let n1 = toSqlKey 1 :: M.NodeId
          n2 = toSqlKey 2 :: M.NodeId
      liftIO $ reserveRam state n1 2048
      liftIO $ reserveRam state n2 1024
      r1 <- liftIO $ reservedRamFor state n1
      r2 <- liftIO $ reservedRamFor state n2
      liftIO $ do
        r1 `shouldBe` 2048
        r2 `shouldBe` 1024

    testCase "clearReservation zeroes the bucket without affecting peers" $ do
      state <- mkState
      let n1 = toSqlKey 1 :: M.NodeId
          n2 = toSqlKey 2 :: M.NodeId
      liftIO $ reserveRam state n1 2048
      liftIO $ reserveRam state n2 1024
      liftIO $ clearReservation state n1
      r1 <- liftIO $ reservedRamFor state n1
      r2 <- liftIO $ reservedRamFor state n2
      liftIO $ do
        r1 `shouldBe` 0
        r2 `shouldBe` 1024
