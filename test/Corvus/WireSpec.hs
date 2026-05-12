-- | Round-trip tests for the pure Cap'n Proto conversion layer.
--
-- These tests cover both directions of the converters in
-- "Corvus.Wire.Enums" and "Corvus.Wire.Common" — encode then decode
-- should be the identity for any well-formed Haskell value. The
-- struct-level converters in @Corvus.Wire.{Vm,Disk,...}@ are
-- exercised through the integration tests once Phase 3 wires them
-- into the server.
module Corvus.WireSpec (spec) where

import qualified Corvus.Model as M
import Corvus.Wire.Common
  ( EntityRef (..)
  , entityRefFromText
  , fromCapnpEntityRef
  , toCapnpEntityRef
  )
import Corvus.Wire.Enums
import Test.Hspec

spec :: Spec
spec = do
  describe "Wire.Enums" $ do
    it "VmStatus round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpVmStatus (toCapnpVmStatus s) `shouldBe` Right s)
        [minBound .. maxBound :: M.VmStatus]
    it "DriveInterface round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpDriveInterface (toCapnpDriveInterface s) `shouldBe` Right s)
        [minBound .. maxBound :: M.DriveInterface]
    it "DriveFormat round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpDriveFormat (toCapnpDriveFormat s) `shouldBe` Right s)
        [minBound .. maxBound :: M.DriveFormat]
    it "DriveMedia round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpDriveMedia (toCapnpDriveMedia s) `shouldBe` Right s)
        [minBound .. maxBound :: M.DriveMedia]
    it "CacheType round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpCacheType (toCapnpCacheType s) `shouldBe` Right s)
        [minBound .. maxBound :: M.CacheType]
    it "NetInterfaceType round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpNetInterfaceType (toCapnpNetInterfaceType s) `shouldBe` Right s)
        [minBound .. maxBound :: M.NetInterfaceType]
    it "SharedDirCache round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpSharedDirCache (toCapnpSharedDirCache s) `shouldBe` Right s)
        [minBound .. maxBound :: M.SharedDirCache]
    it "TemplateCloneStrategy round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpTemplateCloneStrategy (toCapnpTemplateCloneStrategy s) `shouldBe` Right s)
        [minBound .. maxBound :: M.TemplateCloneStrategy]
    it "TaskSubsystem round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpTaskSubsystem (toCapnpTaskSubsystem s) `shouldBe` Right s)
        [minBound .. maxBound :: M.TaskSubsystem]
    it "TaskResult round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpTaskResult (toCapnpTaskResult s) `shouldBe` Right s)
        [minBound .. maxBound :: M.TaskResult]

  describe "Wire.Common.EntityRef" $ do
    it "round-trips numeric ids" $
      fromCapnpEntityRef (toCapnpEntityRef (RefById 42)) `shouldBe` Right (RefById 42)
    it "round-trips symbolic names" $
      fromCapnpEntityRef (toCapnpEntityRef (RefByName "web-1")) `shouldBe` Right (RefByName "web-1")
    it "round-trips empty names" $
      fromCapnpEntityRef (toCapnpEntityRef (RefByName "")) `shouldBe` Right (RefByName "")

  describe "Wire.Common.entityRefFromText (CLI heuristic)" $ do
    it "parses bare digits as id" $
      entityRefFromText "42" `shouldBe` RefById 42
    it "parses zero as id" $
      entityRefFromText "0" `shouldBe` RefById 0
    it "treats negative numbers as names (CLI doesn't address by negative id)" $
      entityRefFromText "-1" `shouldBe` RefByName "-1"
    it "treats mixed-content tokens as names" $
      entityRefFromText "web-1" `shouldBe` RefByName "web-1"
    it "treats plain alphabetic tokens as names" $
      entityRefFromText "webserver" `shouldBe` RefByName "webserver"
    it "treats names beginning with digits as names" $
      entityRefFromText "42web" `shouldBe` RefByName "42web"
