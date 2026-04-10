{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.VmSpec (spec) where

import Corvus.Protocol (Ref (..), Request (..), StatusInfo (..))
import Test.DSL.When (executeRequest)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
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

  describe "vm create" $ do
    testCase "creates a new VM" $ do
      resp <- executeRequest (ReqVmCreate "new-vm" 2 1024 (Just "test") False False False False)
      liftIO $ case resp of
        RespVmCreated vmId -> vmId `shouldSatisfy` (> 0)
        _ -> fail $ "Expected RespVmCreated, got: " ++ show resp
      then_ $ do
        vmCount 1
        vmHasStatus 1 VmStopped

    testCase "creates VM with description" $ do
      resp <- executeRequest (ReqVmCreate "desc-vm" 1 512 (Just "A test VM") False False False False)
      liftIO $ case resp of
        RespVmCreated vmId -> vmId `shouldSatisfy` (> 0)
        _ -> fail $ "Expected RespVmCreated, got: " ++ show resp

  describe "vm delete" $ do
    testCase "deletes a stopped VM" $ do
      given $ do
        _ <- insertVm "delete-me" VmStopped
        pure ()
      resp <- executeRequest (ReqVmDelete (Ref "1"))
      liftIO $ resp `shouldBe` RespVmDeleted
      then_ $ vmNotExists 1

    testCase "fails for running VM" $ do
      given $ do
        _ <- insertVm "running-vm" VmRunning
        pure ()
      resp <- executeRequest (ReqVmDelete (Ref "1"))
      liftIO $ resp `shouldBe` RespVmRunning

    testCase "fails for non-existent VM" $ do
      resp <- executeRequest (ReqVmDelete (Ref "999"))
      liftIO $ resp `shouldBe` RespVmNotFound

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

  describe "vm edit" $ do
    testCase "edits a stopped VM" $ do
      given $ do
        _ <- insertVm "edit-vm" VmStopped
        pure ()
      result <- whenVmEdit 1 (Just 4) (Just 2048) (Just "updated desc") (Just True)
      then_ $ do
        thenVmEdited result
        vmHasStatus 1 VmStopped

    testCase "fails for running VM" $ do
      given $ do
        _ <- insertVm "running-vm" VmRunning
        pure ()
      result <- whenVmEdit 1 (Just 2) Nothing Nothing Nothing
      then_ $ thenVmEditMustBeStopped result

    testCase "fails for paused VM" $ do
      given $ do
        _ <- insertVm "paused-vm" VmPaused
        pure ()
      result <- whenVmEdit 1 Nothing (Just 4096) Nothing Nothing
      then_ $ thenVmEditMustBeStopped result

    testCase "fails for non-existent VM" $ do
      result <- whenVmEdit 999 (Just 2) Nothing Nothing Nothing
      then_ $ thenVmEditNotFound result

  describe "vm state machine" $ do
    testCase "stop fails for paused VM" $ do
      given $ do
        _ <- insertVm "paused-vm" VmPaused
        pure ()
      when_ $ vmStop 1
      then_ responseIsInvalidTransition

    testCase "pause fails for error VM" $ do
      given $ do
        _ <- insertVm "error-vm" VmError
        pure ()
      when_ $ vmPause 1
      then_ responseIsInvalidTransition

    testCase "reset on stopped VM keeps it stopped" $ do
      given $ do
        _ <- insertVm "stopped-vm" VmStopped
        pure ()
      when_ $ vmReset 1
      then_ $ do
        responseIsVmStateChanged
        vmHasStatus 1 VmStopped
