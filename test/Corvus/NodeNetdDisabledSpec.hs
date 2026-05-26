{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Gating around the per-node @netdDisabled@ flag.
--
-- When a node is marked netd-disabled the daemon:
--
-- 1. rejects managed-network creation on the node;
-- 2. rejects @managed@/@tap@/@bridge@/@macvtap@ NICs on VMs placed
--    on the node (only @user@ and @vde@ are accepted);
-- 3. refuses to flip the flag from false to true while the node
--    still owns managed networks or has netd-dependent NICs.
--
-- The supervisor's "skip netd loop" behaviour is exercised
-- elsewhere (it's an IO concern outside the in-memory test DB).
module Corvus.NodeNetdDisabledSpec (spec) where

import qualified Data.Text as T
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "managed-network creation gate" $ do
    testCase "rejects NetworkCreate on a netd-disabled node" $ do
      given $ do
        _ <- seedTestNode
        setTestNodeNetdDisabled True
      when_ $ whenNetworkCreate "n-blocked" "10.0.0.0/24"
      then_ $ responseIs $ \case
        RespNetworkError msg -> "netdDisabled" `T.isInfixOf` msg
        _ -> False

    testCase "still accepts NetworkCreate when the node has netd enabled" $ do
      when_ $ whenNetworkCreate "n-ok" "10.0.0.0/24"
      then_ $ responseIs $ \case
        RespNetworkCreated _ -> True
        _ -> False

  describe "NIC-type gate" $ do
    testCase "rejects NetTap on a netd-disabled node" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        setTestNodeNetdDisabled True
      when_ $ whenNetIfAdd 1 NetTap "tap0" Nothing
      then_ $ responseIs $ \case
        RespError msg -> "requires netd" `T.isInfixOf` msg
        _ -> False

    testCase "rejects NetBridge on a netd-disabled node" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        setTestNodeNetdDisabled True
      when_ $ whenNetIfAdd 1 NetBridge "br0" Nothing
      then_ $ responseIs $ \case
        RespError msg -> "requires netd" `T.isInfixOf` msg
        _ -> False

    testCase "rejects NetMacvtap on a netd-disabled node" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        setTestNodeNetdDisabled True
      when_ $ whenNetIfAdd 1 NetMacvtap "eth0" Nothing
      then_ $ responseIs $ \case
        RespError msg -> "requires netd" `T.isInfixOf` msg
        _ -> False

    testCase "accepts NetUser on a netd-disabled node" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        setTestNodeNetdDisabled True
      when_ $ whenNetIfAdd 1 NetUser "" Nothing
      then_ $ responseIs $ \case
        RespNetIfAdded _ -> True
        _ -> False

    testCase "accepts NetVde on a netd-disabled node" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        setTestNodeNetdDisabled True
      when_ $ whenNetIfAdd 1 NetVde "/run/vde.ctl" Nothing
      then_ $ responseIs $ \case
        RespNetIfAdded _ -> True
        _ -> False

  describe "toggle pre-condition (NodeEdit --netd-disabled true)" $ do
    testCase "allows the flip on a clean node" $ do
      given $ do
        _ <- seedTestNode
        pure ()
      -- Seeded test node has id 1 in a fresh per-test DB.
      when_ $ whenNodeEditNetdDisabled 1 True
      then_ $ responseIs $ \case
        RespNodeEdited -> True
        _ -> False

    testCase "blocks the flip when the node still owns a managed network" $ do
      given $ do
        _ <- insertNetwork "owned-by-node" "10.0.0.0/24"
        pure ()
      when_ $ whenNodeEditNetdDisabled 1 True
      then_ $ responseIs $ \case
        RespError msg ->
          "Cannot disable netd" `T.isInfixOf` msg
            && "managed network" `T.isInfixOf` msg
        _ -> False

    testCase "blocks the flip when a VM on the node has a NetManaged NIC" $ do
      given $ do
        vmId <- insertVm "v" VmStopped
        _ <- insertNetworkInterface vmId NetManaged "" "52:54:00:aa:bb:01"
        pure ()
      when_ $ whenNodeEditNetdDisabled 1 True
      then_ $ responseIs $ \case
        RespError msg ->
          "Cannot disable netd" `T.isInfixOf` msg
            && "netd-dependent type" `T.isInfixOf` msg
        _ -> False

    testCase "allows the flip back to false unconditionally" $ do
      given $ do
        _ <- insertNetwork "owned-by-node" "10.0.0.0/24"
        setTestNodeNetdDisabled True
      -- The flip true→false isn't gated; flipping back is always
      -- legal so operators can recover from accidental toggles.
      when_ $ whenNodeEditNetdDisabled 1 False
      then_ $ responseIs $ \case
        RespNodeEdited -> True
        _ -> False
