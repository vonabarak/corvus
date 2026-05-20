{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the in-memory `corvus-netd` resource ledgers
-- in "Corvus.Netd.Ledger".
--
-- The ledgers are pure STM 'Map's keyed by resource name. We
-- exercise the CRUD shape: empty start, insert, overwrite,
-- delete, no-op delete, isolation between two ledger instances.
-- The polymorphic spec / live types are instantiated as
-- @Text@ in the tests — only the wrapper-level logic is under
-- test here; the concrete spec shapes are exercised by
-- Network / Tap.
module Corvus.NetdLedgerSpec (spec) where

import Control.Concurrent.STM (atomically)
import Corvus.Netd.Ledger
  ( NetworkLedger
  , TapLedger
  , insertNetwork
  , insertTap
  , newNetworkLedger
  , newTapLedger
  , readNetworks
  , readTaps
  , removeNetwork
  , removeTap
  )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

-- | Specialise the polymorphic ledger to 'Text' for these tests
-- — the wrappers are agnostic to the spec / live types, so
-- 'Text' makes assertion bookkeeping trivial.
mkNetworkLedger :: IO (NetworkLedger Text Text)
mkNetworkLedger = newNetworkLedger

mkTapLedger :: IO (TapLedger Text)
mkTapLedger = newTapLedger

spec :: Spec
spec = do
  describe "NetworkLedger" $ do
    it "starts empty" $ do
      l <- mkNetworkLedger
      m <- atomically (readNetworks l)
      m `shouldBe` Map.empty

    it "remembers an inserted entry" $ do
      l <- mkNetworkLedger
      atomically $ insertNetwork l "corvus-br0" "spec-v1" "live-A"
      m <- atomically (readNetworks l)
      Map.lookup "corvus-br0" m `shouldBe` Just ("spec-v1", "live-A")

    it "overwrites an existing entry on re-insert" $ do
      -- Spec change (different VLAN, IP, NAT toggle, …) ends up
      -- here on the netd side; the ledger must reflect the new
      -- pair rather than ignoring the second write.
      l <- mkNetworkLedger
      atomically $ insertNetwork l "corvus-br0" "spec-v1" "live-A"
      atomically $ insertNetwork l "corvus-br0" "spec-v2" "live-B"
      m <- atomically (readNetworks l)
      Map.lookup "corvus-br0" m `shouldBe` Just ("spec-v2", "live-B")

    it "tracks two named entries independently" $ do
      l <- mkNetworkLedger
      atomically $ insertNetwork l "corvus-br0" "s0" "l0"
      atomically $ insertNetwork l "corvus-br1" "s1" "l1"
      m <- atomically (readNetworks l)
      Map.size m `shouldBe` 2
      Map.lookup "corvus-br0" m `shouldBe` Just ("s0", "l0")
      Map.lookup "corvus-br1" m `shouldBe` Just ("s1", "l1")

    it "drops the matched entry on removeNetwork" $ do
      l <- mkNetworkLedger
      atomically $ insertNetwork l "corvus-br0" "s0" "l0"
      atomically $ insertNetwork l "corvus-br1" "s1" "l1"
      atomically $ removeNetwork l "corvus-br0"
      m <- atomically (readNetworks l)
      Map.keys m `shouldBe` ["corvus-br1"]

    it "is a no-op when removeNetwork targets a missing key" $ do
      -- The agent removes by name on shutdown; idempotency
      -- matters because the same name may appear twice (once
      -- when the daemon disconnects, once when the operator
      -- deletes the resource).
      l <- mkNetworkLedger
      atomically $ removeNetwork l "ghost"
      m <- atomically (readNetworks l)
      m `shouldBe` Map.empty

    it "two ledger instances don't share state" $ do
      a <- mkNetworkLedger
      b <- mkNetworkLedger
      atomically $ insertNetwork a "in-a" "s" "l"
      ma <- atomically (readNetworks a)
      mb <- atomically (readNetworks b)
      Map.keys ma `shouldBe` ["in-a"]
      mb `shouldBe` Map.empty

  describe "TapLedger" $ do
    it "starts empty" $ do
      l <- mkTapLedger
      m <- atomically (readTaps l)
      m `shouldBe` Map.empty

    it "remembers an inserted entry and overwrites on re-insert" $ do
      l <- mkTapLedger
      atomically $ insertTap l "corvus-tap-vm42-eth0" "spec-a"
      atomically $ insertTap l "corvus-tap-vm42-eth0" "spec-b"
      m <- atomically (readTaps l)
      Map.lookup "corvus-tap-vm42-eth0" m `shouldBe` Just "spec-b"

    it "drops the matched entry on removeTap and is a no-op on misses" $ do
      l <- mkTapLedger
      atomically $ insertTap l "corvus-tap-vm42-eth0" "spec-a"
      atomically $ removeTap l "corvus-tap-vm42-eth0"
      atomically $ removeTap l "ghost"
      m <- atomically (readTaps l)
      m `shouldBe` Map.empty
