{-# LANGUAGE OverloadedStrings #-}

module Corvus.VmTransitionSpec (spec) where

import Corvus.Model (VmStatus (..))
import Corvus.Model.VmState (VmAction (..), validateTransition)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "validateTransition (operator actions)" $ do
    -- From Stopped
    it "stopped + startCold = running" $
      validateTransition VmStopped ActionStartCold `shouldBe` Right VmRunning
    it "stopped + startColdWithGA = starting" $
      validateTransition VmStopped ActionStartColdWithGA `shouldBe` Right VmStarting
    it "stopped + reset = stopped" $
      validateTransition VmStopped ActionReset `shouldBe` Right VmStopped
    it "stopped + stop = error" $
      validateTransition VmStopped ActionStop `shouldSatisfy` isLeft
    it "stopped + pause = error" $
      validateTransition VmStopped ActionPause `shouldSatisfy` isLeft
    it "stopped + save = error" $
      validateTransition VmStopped ActionSave `shouldSatisfy` isLeft

    -- From Starting
    it "starting + stop = stopping" $
      validateTransition VmStarting ActionStop `shouldBe` Right VmStopping
    it "starting + reset = stopped" $
      validateTransition VmStarting ActionReset `shouldBe` Right VmStopped
    it "starting + startCold = error" $
      validateTransition VmStarting ActionStartCold `shouldSatisfy` isLeft
    it "starting + pause = error" $
      validateTransition VmStarting ActionPause `shouldSatisfy` isLeft

    -- From Running
    it "running + stop = stopping" $
      validateTransition VmRunning ActionStop `shouldBe` Right VmStopping
    it "running + pause = paused" $
      validateTransition VmRunning ActionPause `shouldBe` Right VmPaused
    it "running + reset = stopped" $
      validateTransition VmRunning ActionReset `shouldBe` Right VmStopped
    it "running + startCold = error" $
      validateTransition VmRunning ActionStartCold `shouldSatisfy` isLeft
    it "running + save = saving" $
      validateTransition VmRunning ActionSave `shouldBe` Right VmSaving

    -- From Stopping
    it "stopping + reset = stopped" $
      validateTransition VmStopping ActionReset `shouldBe` Right VmStopped
    it "stopping + startCold = error" $
      validateTransition VmStopping ActionStartCold `shouldSatisfy` isLeft
    it "stopping + stop = error" $
      validateTransition VmStopping ActionStop `shouldSatisfy` isLeft
    it "stopping + pause = error" $
      validateTransition VmStopping ActionPause `shouldSatisfy` isLeft

    -- From Paused
    it "paused + startResumePaused = running (QMP cont)" $
      validateTransition VmPaused ActionStartResumePaused `shouldBe` Right VmRunning
    it "paused + reset = stopped" $
      validateTransition VmPaused ActionReset `shouldBe` Right VmStopped
    it "paused + stop = error" $
      validateTransition VmPaused ActionStop `shouldSatisfy` isLeft
    it "paused + pause = error" $
      validateTransition VmPaused ActionPause `shouldSatisfy` isLeft
    it "paused + save = saving" $
      validateTransition VmPaused ActionSave `shouldBe` Right VmSaving
    it "paused + startCold = error" $
      validateTransition VmPaused ActionStartCold `shouldSatisfy` isLeft
    it "paused + startResumeSaved = error" $
      validateTransition VmPaused ActionStartResumeSaved `shouldSatisfy` isLeft

    -- From Saved.
    it "saved + startResumeSaved = loading (the agent will spawn QEMU with -incoming)" $
      validateTransition VmSaved ActionStartResumeSaved `shouldBe` Right VmLoading
    it "saved + reset = stopped" $
      validateTransition VmSaved ActionReset `shouldBe` Right VmStopped
    it "saved + stop = error (points at start / reset)" $ do
      let r = validateTransition VmSaved ActionStop
      r `shouldSatisfy` isLeft
      case r of
        Left msg -> do
          msg `shouldSatisfy` T.isInfixOf "start"
          msg `shouldSatisfy` T.isInfixOf "reset"
        Right _ -> expectationFailure "expected Left"
    it "saved + pause = error" $
      validateTransition VmSaved ActionPause `shouldSatisfy` isLeft
    it "saved + save = error (already saved)" $
      validateTransition VmSaved ActionSave `shouldSatisfy` isLeft
    it "saved + startCold = error" $
      validateTransition VmSaved ActionStartCold `shouldSatisfy` isLeft
    it "saved + startResumePaused = error" $
      validateTransition VmSaved ActionStartResumePaused `shouldSatisfy` isLeft

    -- From Error
    it "error + reset = stopped" $
      validateTransition VmError ActionReset `shouldBe` Right VmStopped
    it "error + startCold = error" $
      validateTransition VmError ActionStartCold `shouldSatisfy` isLeft
    it "error + stop = error" $
      validateTransition VmError ActionStop `shouldSatisfy` isLeft
    it "error + pause = error" $
      validateTransition VmError ActionPause `shouldSatisfy` isLeft
    it "error + save = error" $
      validateTransition VmError ActionSave `shouldSatisfy` isLeft

  describe "validateTransition (transient states — operator actions blocked, reset escapes)" $ do
    -- From Saving
    it "saving + reset = stopped (cancel)" $
      validateTransition VmSaving ActionReset `shouldBe` Right VmStopped
    it "saving + startCold = error (in-flight)" $
      validateTransition VmSaving ActionStartCold `shouldSatisfy` isLeft
    it "saving + startResumeSaved = error (in-flight)" $
      validateTransition VmSaving ActionStartResumeSaved `shouldSatisfy` isLeft
    it "saving + stop = error (in-flight)" $
      validateTransition VmSaving ActionStop `shouldSatisfy` isLeft
    it "saving + pause = error (in-flight)" $
      validateTransition VmSaving ActionPause `shouldSatisfy` isLeft
    it "saving + save = error (in-flight)" $
      validateTransition VmSaving ActionSave `shouldSatisfy` isLeft

    -- From Loading
    it "loading + reset = stopped (cancel)" $
      validateTransition VmLoading ActionReset `shouldBe` Right VmStopped
    it "loading + startCold = error (in-flight)" $
      validateTransition VmLoading ActionStartCold `shouldSatisfy` isLeft
    it "loading + stop = error (in-flight)" $
      validateTransition VmLoading ActionStop `shouldSatisfy` isLeft
    it "loading + pause = error (in-flight)" $
      validateTransition VmLoading ActionPause `shouldSatisfy` isLeft
    it "loading + save = error (in-flight)" $
      validateTransition VmLoading ActionSave `shouldSatisfy` isLeft

    -- From Migrating
    it "migrating + reset = stopped (cancel)" $
      validateTransition VmMigrating ActionReset `shouldBe` Right VmStopped
    it "migrating + startCold = error (in-flight)" $
      validateTransition VmMigrating ActionStartCold `shouldSatisfy` isLeft
    it "migrating + stop = error (in-flight)" $
      validateTransition VmMigrating ActionStop `shouldSatisfy` isLeft
    it "migrating + pause = error (in-flight)" $
      validateTransition VmMigrating ActionPause `shouldSatisfy` isLeft
    it "migrating + save = error (in-flight)" $
      validateTransition VmMigrating ActionSave `shouldSatisfy` isLeft

  describe "validateTransition (system-completion actions)" $ do
    -- Save worker completes
    it "saving + SaveDone = saved" $
      validateTransition VmSaving ActionSaveDone `shouldBe` Right VmSaved
    it "saving + SaveFail = error" $
      validateTransition VmSaving ActionSaveFail `shouldBe` Right VmError
    it "saveDone from non-saving = error" $
      validateTransition VmRunning ActionSaveDone `shouldSatisfy` isLeft
    it "saveFail from non-saving = error" $
      validateTransition VmStopped ActionSaveFail `shouldSatisfy` isLeft

    -- Load worker completes
    it "loading + LoadDone = running" $
      validateTransition VmLoading ActionLoadDone `shouldBe` Right VmRunning
    it "loading + LoadFail = error" $
      validateTransition VmLoading ActionLoadFail `shouldBe` Right VmError
    it "loadDone from non-loading = error" $
      validateTransition VmRunning ActionLoadDone `shouldSatisfy` isLeft

    -- Migrate orchestrator completes
    it "migrating + MigrateDone = saved" $
      validateTransition VmMigrating ActionMigrateDone `shouldBe` Right VmSaved
    it "migrating + MigrateFail = saved (rollback preserves source state)" $
      validateTransition VmMigrating ActionMigrateFail `shouldBe` Right VmSaved
    it "migrateDone from non-migrating = error" $
      validateTransition VmRunning ActionMigrateDone `shouldSatisfy` isLeft

  describe "validateTransition (properties)" $ do
    -- These properties guard against two classes of regression:
    -- 1. A future 'case _ -> Right currentStatus' wildcard that masks
    --    an explicitly-invalid transition.
    -- 2. A broken 'ActionReset' rule. Reset is the sole escape hatch
    --    from every state; if it ever stops returning VmStopped, an
    --    operator can't recover a wedged VM.

    it "every transition returns either a Right newStatus or a Left with a non-empty message" $
      property $ \status action -> case validateTransition status action of
        Right _ -> True
        Left msg -> not (T.null msg)

    it "ActionReset is always allowed and always lands in VmStopped" $
      property $ \status ->
        validateTransition status ActionReset === Right VmStopped

    it "validateTransition is total: no pair produces an exception" $
      property $ \status action ->
        validateTransition status action `seq` True

-- Arbitrary instances for exhaustive coverage of the
-- (VmStatus, VmAction) space. Both derive Enum+Bounded so we use
-- arbitraryBoundedEnum to enumerate every constructor.
instance Arbitrary VmStatus where
  arbitrary = arbitraryBoundedEnum
  shrink _ = []

instance Arbitrary VmAction where
  arbitrary = arbitraryBoundedEnum
  shrink _ = []

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
