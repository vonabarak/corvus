{-# LANGUAGE OverloadedStrings #-}

module Corvus.VmTransitionSpec (spec) where

import Corvus.Handlers.Vm (VmAction (..), validateTransition)
import Corvus.Model (VmStatus (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "validateTransition" $ do
    -- Valid transitions from Stopped
    it "stopped + start = running" $
      validateTransition VmStopped ActionStart `shouldBe` Right VmRunning
    it "stopped + reset = stopped" $
      validateTransition VmStopped ActionReset `shouldBe` Right VmStopped
    it "stopped + stop = error" $
      validateTransition VmStopped ActionStop `shouldSatisfy` isLeft
    it "stopped + pause = error" $
      validateTransition VmStopped ActionPause `shouldSatisfy` isLeft

    -- Valid transitions from Starting
    it "starting + stop = stopping" $
      validateTransition VmStarting ActionStop `shouldBe` Right VmStopping
    it "starting + reset = stopped" $
      validateTransition VmStarting ActionReset `shouldBe` Right VmStopped
    it "starting + start = error" $
      validateTransition VmStarting ActionStart `shouldSatisfy` isLeft
    it "starting + pause = error" $
      validateTransition VmStarting ActionPause `shouldSatisfy` isLeft

    -- Valid transitions from Running
    it "running + stop = stopping" $
      validateTransition VmRunning ActionStop `shouldBe` Right VmStopping
    it "running + pause = paused" $
      validateTransition VmRunning ActionPause `shouldBe` Right VmPaused
    it "running + reset = stopped" $
      validateTransition VmRunning ActionReset `shouldBe` Right VmStopped
    it "running + start = error" $
      validateTransition VmRunning ActionStart `shouldSatisfy` isLeft

    -- Valid transitions from Stopping
    it "stopping + reset = stopped" $
      validateTransition VmStopping ActionReset `shouldBe` Right VmStopped
    it "stopping + start = error" $
      validateTransition VmStopping ActionStart `shouldSatisfy` isLeft
    it "stopping + stop = error" $
      validateTransition VmStopping ActionStop `shouldSatisfy` isLeft
    it "stopping + pause = error" $
      validateTransition VmStopping ActionPause `shouldSatisfy` isLeft

    -- Valid transitions from Paused
    it "paused + start = running (resume)" $
      validateTransition VmPaused ActionStart `shouldBe` Right VmRunning
    it "paused + reset = stopped" $
      validateTransition VmPaused ActionReset `shouldBe` Right VmStopped
    it "paused + stop = error" $
      validateTransition VmPaused ActionStop `shouldSatisfy` isLeft
    it "paused + pause = error" $
      validateTransition VmPaused ActionPause `shouldSatisfy` isLeft

    -- Valid transitions from Error
    it "error + reset = stopped" $
      validateTransition VmError ActionReset `shouldBe` Right VmStopped
    it "error + start = error" $
      validateTransition VmError ActionStart `shouldSatisfy` isLeft
    it "error + stop = error" $
      validateTransition VmError ActionStop `shouldSatisfy` isLeft
    it "error + pause = error" $
      validateTransition VmError ActionPause `shouldSatisfy` isLeft

    -- Reset is always allowed (any state)
    it "any + reset = stopped" $ do
      validateTransition VmStopped ActionReset `shouldBe` Right VmStopped
      validateTransition VmStarting ActionReset `shouldBe` Right VmStopped
      validateTransition VmRunning ActionReset `shouldBe` Right VmStopped
      validateTransition VmStopping ActionReset `shouldBe` Right VmStopped
      validateTransition VmPaused ActionReset `shouldBe` Right VmStopped
      validateTransition VmError ActionReset `shouldBe` Right VmStopped

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
