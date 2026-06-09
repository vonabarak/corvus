{-# LANGUAGE OverloadedStrings #-}

-- | Pure-side coverage of the live-snapshot plumbing — no
-- DB, no daemon, no agent. Confirms 'Corvus.Wire.Disk' round-
-- trips the @live@ / @quiesced@ / @hasVmstate@ fields on
-- 'SnapshotInfo' across every permutation.
module Corvus.SnapshotLiveSpec (spec) where

import Corvus.Protocol (SnapshotInfo (..))
import qualified Corvus.Wire.Disk as W
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Test.Hspec

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2026 6 5) (secondsToDiffTime 12345)

mkInfo :: Bool -> Bool -> Bool -> SnapshotInfo
mkInfo live quiesced hasVmstate =
  SnapshotInfo
    { sniId = 42
    , sniName = "cache-step-3"
    , sniCreatedAt = sampleTime
    , sniSizeMb = Just 128
    , sniLive = live
    , sniQuiesced = quiesced
    , sniHasVmstate = hasVmstate
    }

spec :: Spec
spec = describe "SnapshotInfo wire round-trip carries live/quiesced/hasVmstate" $ do
  let bools = [False, True]
      cases = [(l, q, v) | l <- bools, q <- bools, v <- bools]
  mapM_
    ( \(live, quiesced, hasVmstate) ->
        it
          ( "live="
              <> show live
              <> ", quiesced="
              <> show quiesced
              <> ", hasVmstate="
              <> show hasVmstate
              <> " survives toCapnp/fromCapnp"
          )
          $ do
            let original = mkInfo live quiesced hasVmstate
                roundTripped =
                  W.fromCapnpSnapshotInfo (W.toCapnpSnapshotInfo original)
            roundTripped `shouldBe` original
    )
    cases
