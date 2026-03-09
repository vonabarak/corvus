{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.VmSpec (spec) where

import Test.Prelude

spec :: Spec
spec = withTestDb $ do
  describe "vm list" $ do
    testCase "returns empty list when no VMs exist" $ do
      when_ vmList
      then_ $ responseIs $ \case
        RespVmList [] -> True
        _ -> False

    testCase "returns all VMs" $ do
      given $ do
        _ <- insertVm "vm1" VmStopped
        _ <- insertVm "vm2" VmRunning
        pure ()
      when_ vmList
      then_ $ responseIs $ \case
        RespVmList vms -> length vms == 2
        _ -> False

  describe "vm show" $ do
    testCase "returns VM details for existing VM" $ do
      given $ do
        _ <- insertVm "test-vm" VmStopped
        pure ()
      when_ $ vmShow 1
      then_ $ responseIs $ \case
        RespVmDetails _ -> True
        _ -> False

    testCase "returns not found for non-existent VM" $ do
      when_ $ vmShow 999
      then_ responseIsVmNotFound

  describe "vm start" $ do
    testCase "fails for non-existent VM" $ do
      when_ $ vmStart 999
      then_ responseIsVmNotFound

    testCase "fails for already running VM" $ do
      given $ do
        _ <- insertVm "running-vm" VmRunning
        pure ()
      when_ $ vmStart 1
      then_ responseIsInvalidTransition

    testCase "fails for VM in error state" $ do
      given $ do
        _ <- insertVm "error-vm" VmError
        pure ()
      when_ $ vmStart 1
      then_ responseIsInvalidTransition

  describe "vm stop" $ do
    testCase "fails for non-existent VM" $ do
      when_ $ vmStop 999
      then_ responseIsVmNotFound

    testCase "fails for already stopped VM" $ do
      given $ do
        _ <- insertVm "stopped-vm" VmStopped
        pure ()
      when_ $ vmStop 1
      then_ responseIsInvalidTransition

  describe "vm pause" $ do
    testCase "fails for non-existent VM" $ do
      when_ $ vmPause 999
      then_ responseIsVmNotFound

    testCase "fails for stopped VM" $ do
      given $ do
        _ <- insertVm "stopped-vm" VmStopped
        pure ()
      when_ $ vmPause 1
      then_ responseIsInvalidTransition

  describe "vm reset" $ do
    testCase "fails for non-existent VM" $ do
      when_ $ vmReset 999
      then_ responseIsVmNotFound

    testCase "resets VM in error state to stopped" $ do
      given $ do
        _ <- insertVm "error-vm" VmError
        pure ()
      when_ $ vmReset 1
      then_ $ do
        responseIsVmStateChanged
        vmHasStatus 1 VmStopped
