{-# LANGUAGE OverloadedStrings #-}

-- | Pure-side coverage of the live-snapshot plumbing — no
-- DB, no daemon, no agent. Confirms 'Corvus.Wire.Disk' round-
-- trips the new @live@ / @quiesced@ fields on 'SnapshotInfo'
-- for all four permutations.
module Corvus.SnapshotLiveSpec (spec) where

import Corvus.Protocol (SnapshotInfo (..))
import qualified Corvus.Wire.Disk as W
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Test.Hspec

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2026 6 5) (secondsToDiffTime 12345)

mkInfo :: Bool -> Bool -> SnapshotInfo
mkInfo live quiesced =
  SnapshotInfo
    { sniId = 42
    , sniName = "cache-step-3"
    , sniCreatedAt = sampleTime
    , sniSizeMb = Just 128
    , sniLive = live
    , sniQuiesced = quiesced
    }

spec :: Spec
spec = describe "SnapshotInfo wire round-trip carries live/quiesced" $ do
  let cases =
        [ (False, False)
        , (True, False)
        , (False, True)
        , (True, True)
        ]
  mapM_
    ( \(live, quiesced) ->
        it
          ( "live="
              <> show live
              <> ", quiesced="
              <> show quiesced
              <> " survives toCapnp/fromCapnp"
          )
          $ do
            let original = mkInfo live quiesced
                roundTripped =
                  W.fromCapnpSnapshotInfo (W.toCapnpSnapshotInfo original)
            roundTripped `shouldBe` original
    )
    cases
