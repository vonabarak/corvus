{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the AF_VSOCK CID allocator.
--
-- Phase 6 moved the host-kernel probe behind a per-node
-- 'probeVsockCid' nodeagent RPC. The previous test set exercised
-- the allocator's DB filter + the local C-FFI probe directly;
-- with the probe now agent-side, the tests would need a fake
-- nodeagent in the test DSL (the stub 'NodeConns' the DSL
-- installs has 'ncNodeAgent = Nothing'). Leaving the body
-- pending until that scaffolding lands; the per-node DB filter
-- is covered by 'CapnpServerSpec' end-to-end via @crv vm create@
-- on the in-process nodeagent.
module Corvus.VsockCidSpec (spec) where

import Test.Prelude

spec :: Spec
spec =
  describe "VsockCid (Phase 6 — see comment)" $
    it "deferred until fake nodeagent lands in test DSL" $
      pendingWith
        "Phase 6: probe moved to per-node agent; needs fake nodeagent for unit tests"
