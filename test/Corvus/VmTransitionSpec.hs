{-# LANGUAGE OverloadedStrings #-}

module Corvus.VmTransitionSpec (spec) where

import Corvus.Handlers.Vm (VmAction (..), validateTransition)
import Corvus.Model (VmStatus (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "validateTransition" $ do
    -- Valid transitions
    it "stopped + start = running" $
      validateTransition VmStopped ActionStart `shouldBe` Right VmRunning
    it "running + stop = stopped" $
      validateTransition VmRunning ActionStop `shouldBe` Right VmStopped
    it "running + pause = paused" $
      validateTransition VmRunning ActionPause `shouldBe` Right VmPaused
    it "paused + start = running (resume)" $
      validateTransition VmPaused ActionStart `shouldBe` Right VmRunning
    it "any + reset = stopped" $ do
      validateTransition VmStopped ActionReset `shouldBe` Right VmStopped
      validateTransition VmRunning ActionReset `shouldBe` Right VmStopped
      validateTransition VmPaused ActionReset `shouldBe` Right VmStopped
      validateTransition VmError ActionReset `shouldBe` Right VmStopped

    -- Invalid transitions
    it "stopped + stop = error" $
      validateTransition VmStopped ActionStop `shouldSatisfy` isLeft
    it "stopped + pause = error" $
      validateTransition VmStopped ActionPause `shouldSatisfy` isLeft
    it "running + start = error" $
      validateTransition VmRunning ActionStart `shouldSatisfy` isLeft
    it "paused + stop = error" $
      validateTransition VmPaused ActionStop `shouldSatisfy` isLeft
    it "paused + pause = error" $
      validateTransition VmPaused ActionPause `shouldSatisfy` isLeft
    it "error + start = error" $
      validateTransition VmError ActionStart `shouldSatisfy` isLeft
    it "error + stop = error" $
      validateTransition VmError ActionStop `shouldSatisfy` isLeft
    it "error + pause = error" $
      validateTransition VmError ActionPause `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
