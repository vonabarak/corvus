{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Guest-exec command validation paths.
--
-- The actual QGA round-trip lives in `Corvus.Node.GuestAgent`
-- (exercised by `Corvus.GuestAgentSpec` and the integration
-- suite). What `handleGuestExec` itself does is gate the call:
-- VM must exist, must be running, and must have its guest-agent
-- bit set. We exercise each of those guards plus the
-- agent-unavailable branch (the test fixture's stub
-- `NodeConns` has `ncNodeAgent = Nothing`).
module Corvus.GuestExecSpec (spec) where

import Corvus.Model (VmStatus (..))
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "whenGuestExec" $ do
    testCase "returns VmNotFound for an unknown VM" $ do
      when_ $ whenGuestExec 999 "true"
      then_ responseIsVmNotFound

    testCase "refuses against a stopped VM" $ do
      given $ do
        _ <- insertVm "stopped" VmStopped
        pure ()
      when_ $ whenGuestExec 1 "true"
      then_ $ responseIs $ \case
        RespInvalidTransition VmStopped _ -> True
        _ -> False

    testCase "refuses when guest-agent isn't enabled on the VM" $ do
      given $ do
        -- insertVm leaves vmGuestAgent=False; the handler should
        -- short-circuit on that even after the status check passes.
        _ <- insertVm "no-ga" VmRunning
        pure ()
      when_ $ whenGuestExec 1 "true"
      then_ $ responseIs $ \case
        RespGuestAgentNotEnabled -> True
        _ -> False

    testCase "surfaces an agent error when the nodeagent isn't dialled" $ do
      -- A running VM with guestAgent=True funnels through the
      -- per-node agent dial. The stub NodeConns has no agent
      -- attached → handler returns RespGuestAgentError.
      given $ do
        _ <- insertRunningVmWithGuestAgent "running-ga"
        pure ()
      when_ $ whenGuestExec 1 "true"
      then_ $ responseIs $ \case
        RespGuestAgentError _ -> True
        _ -> False
