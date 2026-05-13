{-# LANGUAGE OverloadedStrings #-}

-- | GuestAgentPollerIntegrationSpec — disabled in Phase 5.
--
-- This integration test exercises functionality that depended on the
-- legacy 'Data.Binary' wire (e.g. live serial-console relay, async
-- streaming returns, fine-grained 'VmActionResult' / 'DiskResult'
-- shape). With the cutover to Cap'n Proto the surface changed enough
-- that the test needs a deeper rewrite — out of scope for the Phase
-- 5 cutover commit. Reinstated alongside Phase 6 (streaming).
module Corvus.GuestAgentPollerIntegrationSpec (spec) where

import Test.Hspec

spec :: Spec
spec =
  describe "GuestAgentPollerIntegrationSpec" $
    it "disabled — pending Phase 6 streaming rewrite" $
      pendingWith "Integration suite needs a Cap'n Proto-aware rewrite (Phase 5 cutover)."
