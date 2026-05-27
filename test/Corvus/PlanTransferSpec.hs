{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the pure path-resolution layer of
-- @disk copy@ / @disk move@.
--
-- 'Corvus.Handlers.Disk.computeDestPaths' captures the
-- decision matrix the daemon uses to pick the destination path
-- on a `crv disk copy` / `crv disk move` operation. Behaviour
-- under test, from the user spec:
--
--   * If the source's stored path is **absolute** and no
--     @--to-path@ is supplied → refuse (the same absolute path
--     on a different node is rarely writable and almost never
--     the intent).
--   * If the source's stored path is **relative** and no
--     @--to-path@ is supplied → preserve the same relative
--     path verbatim (so @templates/ubuntu.qcow2@ stays at
--     @templates/ubuntu.qcow2@ on the destination, not flattened
--     to @ubuntu.qcow2@).
--   * If @--to-path@ is supplied → run it through
--     'Corvus.Handlers.Disk.Path.resolveDiskFilePathPure' (so
--     trailing-@/@ means "directory, pick the source basename")
--     anchored at the destination node's @basePath@, then
--     normalise the storage form via 'makeRelativeToBase'.
--
-- These tests don't need a DB or an agent — the function is
-- pure. Round-trip vs. 'resolveDiskPath' is asserted by
-- comparing the stored form against the absolute we'd later
-- compute from it.
module Corvus.PlanTransferSpec (spec) where

import Corvus.Handlers.Disk (computeDestPaths)
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = describe "computeDestPaths" $ do
  let destBase = "/home/kvm/VMs"

  describe "absolute source path" $ do
    it "refuses without --to-path, error mentions 'absolute' and '--to-path'" $ do
      let result =
            computeDestPaths
              True
              "/srv/images/foo.qcow2"
              "/srv/images/foo.qcow2"
              destBase
              Nothing
      case result of
        Left err -> do
          T.unpack err `shouldContain` "absolute"
          T.unpack err `shouldContain` "--to-path"
        Right _ -> expectationFailure "expected Left, got Right"

    it "accepts --to-path with a relative destination, anchors to destBase" $ do
      let result =
            computeDestPaths
              True
              "/srv/images/foo.qcow2"
              "/srv/images/foo.qcow2"
              destBase
              (Just "relocated.qcow2")
      result `shouldBe` Right ("/home/kvm/VMs/relocated.qcow2", "relocated.qcow2")

    it "accepts --to-path with an absolute destination (stored verbatim)" $ do
      let result =
            computeDestPaths
              True
              "/srv/images/foo.qcow2"
              "/srv/images/foo.qcow2"
              destBase
              (Just "/data/clone.qcow2")
      result `shouldBe` Right ("/data/clone.qcow2", "/data/clone.qcow2")

  describe "relative source path (no --to-path)" $ do
    it "preserves a simple basename" $ do
      let result =
            computeDestPaths
              False
              "foo.qcow2"
              "/home/kvm/VMs/foo.qcow2"
              destBase
              Nothing
      result `shouldBe` Right ("/home/kvm/VMs/foo.qcow2", "foo.qcow2")

    it "preserves a nested subdirectory" $ do
      let result =
            computeDestPaths
              False
              "templates/ubuntu-24.qcow2"
              "/home/kvm/VMs/templates/ubuntu-24.qcow2"
              destBase
              Nothing
      result
        `shouldBe` Right
          ( "/home/kvm/VMs/templates/ubuntu-24.qcow2"
          , "templates/ubuntu-24.qcow2"
          )

    it "preserves a deeply nested path" $ do
      let result =
            computeDestPaths
              False
              "nested/deep/x.qcow2"
              "/home/kvm/VMs/nested/deep/x.qcow2"
              destBase
              Nothing
      result
        `shouldBe` Right
          ( "/home/kvm/VMs/nested/deep/x.qcow2"
          , "nested/deep/x.qcow2"
          )

  describe "relative source path (with --to-path)" $ do
    it "overrides the storage path when given a filename" $ do
      let result =
            computeDestPaths
              False
              "templates/ubuntu-24.qcow2"
              "/home/kvm/VMs/templates/ubuntu-24.qcow2"
              destBase
              (Just "staging/x.qcow2")
      result
        `shouldBe` Right
          ( "/home/kvm/VMs/staging/x.qcow2"
          , "staging/x.qcow2"
          )

    it "treats trailing slash as 'directory + source basename'" $ do
      let result =
            computeDestPaths
              False
              "templates/ubuntu-24.qcow2"
              "/home/kvm/VMs/templates/ubuntu-24.qcow2"
              destBase
              (Just "archive/")
      -- The source basename is "ubuntu-24.qcow2" — it becomes
      -- the filename appended after the directory.
      result
        `shouldBe` Right
          ( "/home/kvm/VMs/archive/ubuntu-24.qcow2"
          , "archive/ubuntu-24.qcow2"
          )

    it "stores absolute paths outside destBase verbatim" $ do
      let result =
            computeDestPaths
              False
              "foo.qcow2"
              "/home/kvm/VMs/foo.qcow2"
              destBase
              (Just "/tmp/explicit.qcow2")
      result `shouldBe` Right ("/tmp/explicit.qcow2", "/tmp/explicit.qcow2")

    it "stores absolute paths inside destBase as relative" $ do
      let result =
            computeDestPaths
              False
              "foo.qcow2"
              "/home/kvm/VMs/foo.qcow2"
              destBase
              (Just "/home/kvm/VMs/sub/x.qcow2")
      -- makeRelativeToBase strips the basePath prefix so
      -- subsequent resolveDiskPath calls round-trip back to the
      -- same absolute path via destBase.
      result
        `shouldBe` Right
          ( "/home/kvm/VMs/sub/x.qcow2"
          , "sub/x.qcow2"
          )

  describe "basePath edge cases" $ do
    -- A node registered with a trailing slash on its basePath
    -- (e.g. `/home/kvm/VMs/`) used to round-trip wrong: the
    -- relative-path stripper only matched `<basePath>/`, so a
    -- trailing-slash basePath produced `<basePath>//` which
    -- never prefix-matched, and the storage form stayed
    -- absolute. Regression-guard the normalisation.
    it "handles basePath with a trailing slash on the destination" $ do
      let result =
            computeDestPaths
              False
              "foo.qcow2"
              "/home/kvm/VMs/foo.qcow2"
              "/home/kvm/VMs/"
              (Just "staging/x.qcow2")
      result `shouldBe` Right ("/home/kvm/VMs/staging/x.qcow2", "staging/x.qcow2")

    it "handles basePath with multiple trailing slashes" $ do
      let result =
            computeDestPaths
              False
              "foo.qcow2"
              "/home/kvm/VMs/foo.qcow2"
              "/home/kvm/VMs///"
              (Just "staging/x.qcow2")
      result `shouldBe` Right ("/home/kvm/VMs/staging/x.qcow2", "staging/x.qcow2")
