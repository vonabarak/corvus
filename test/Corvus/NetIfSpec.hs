{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Network-interface CRUD for VMs.
--
-- The handler under test is `Corvus.Handlers.NetIf`. NetIfs hang
-- off a VM row — every helper takes a `vmId`; missing VM →
-- VmNotFound. A MAC is auto-generated when the caller doesn't
-- pass one; we assert on the auto-generation by checking
-- `whenNetIfList` afterwards.
module Corvus.NetIfSpec (spec) where

import Corvus.Protocol (NetIfInfo (..))
import qualified Data.Text as T
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "whenNetIfList" $ do
    testCase "returns VmNotFound for an unknown VM" $ do
      when_ $ whenNetIfList 999
      then_ responseIsVmNotFound

    testCase "returns an empty list for a VM with no interfaces" $ do
      given $ do
        _ <- insertVm "bare" VmStopped
        pure ()
      when_ $ whenNetIfList 1
      then_ $ responseIs $ \case
        RespNetIfList [] -> True
        _ -> False

    testCase "returns each inserted interface" $ do
      given $ do
        vmId <- insertVm "v" VmStopped
        _ <- insertNetworkInterface vmId NetUser "" "52:54:00:aa:bb:01"
        _ <- insertNetworkInterface vmId NetUser "" "52:54:00:aa:bb:02"
        pure ()
      when_ $ whenNetIfList 1
      then_ $ responseIs $ \case
        RespNetIfList xs -> length xs == 2
        _ -> False

  describe "whenNetIfAdd" $ do
    testCase "returns VmNotFound when the VM doesn't exist" $ do
      when_ $ whenNetIfAdd 999 NetUser "" Nothing
      then_ responseIsVmNotFound

    testCase "happy path: writes a row and auto-generates a MAC" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        pure ()
      when_ $ whenNetIfAdd 1 NetUser "" Nothing
      then_ $ do
        responseIs $ \case
          RespNetIfAdded _ -> True
          _ -> False
      -- A fresh row must be readable + carry a non-empty MAC.
      when_ $ whenNetIfList 1
      then_ $ responseIs $ \case
        RespNetIfList [n] -> not (T.null (niMacAddress n))
        _ -> False

    testCase "uses the caller's MAC verbatim when one is supplied" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        pure ()
      when_ $ whenNetIfAdd 1 NetUser "" (Just "52:54:00:de:ad:01")
      when_ $ whenNetIfList 1
      then_ $ responseIs $ \case
        RespNetIfList [n] -> niMacAddress n == "52:54:00:de:ad:01"
        _ -> False

    testCase "bridge type requires --host-device" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        pure ()
      when_ $ whenNetIfAdd 1 NetBridge "" Nothing
      then_ $ responseIs $ \case
        RespError msg -> "bridge interface requires" `T.isInfixOf` msg
        _ -> False
      -- And nothing got inserted.
      when_ $ whenNetIfList 1
      then_ $ responseIs $ \case
        RespNetIfList [] -> True
        _ -> False

    testCase "bridge type accepts a non-empty host bridge name" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        pure ()
      when_ $ whenNetIfAdd 1 NetBridge "br0" Nothing
      then_ $ responseIs $ \case
        RespNetIfAdded _ -> True
        _ -> False
      when_ $ whenNetIfList 1
      then_ $ responseIs $ \case
        RespNetIfList [n] ->
          niType n == NetBridge && niHostDevice n == "br0"
        _ -> False

  describe "whenNetIfRemove" $ do
    testCase "returns VmNotFound when the VM doesn't exist" $ do
      when_ $ whenNetIfRemove 999 1
      then_ responseIsVmNotFound

    testCase "returns NetIfNotFound when the interface doesn't exist" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        pure ()
      when_ $ whenNetIfRemove 1 999
      then_ $ responseIs $ \case
        RespNetIfNotFound -> True
        _ -> False

    testCase "removes the row when both VM + interface exist" $ do
      given $ do
        vmId <- insertVm "v" VmStopped
        _ <- insertNetworkInterface vmId NetUser "" "52:54:00:fe:ed:01"
        pure ()
      when_ $ whenNetIfRemove 1 1
      then_ $ do
        responseIs (== RespOk)
      when_ $ whenNetIfList 1
      then_ $ responseIs $ \case
        RespNetIfList [] -> True
        _ -> False
