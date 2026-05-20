{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared-directory CRUD per VM.
--
-- The handler under test is `Corvus.Handlers.SharedDir`. Shared
-- dirs are tagged virtiofs mounts; the daemon-side row enforces a
-- unique `(vm_id, tag)` constraint. The handler also rejects
-- invalid tags before touching the DB and surfaces `VmNotFound`
-- when the parent VM doesn't exist.
module Corvus.SharedDirSpec (spec) where

import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "whenSharedDirList" $ do
    testCase "returns VmNotFound for an unknown VM" $ do
      when_ $ whenSharedDirList 999
      then_ responseIsVmNotFound

    testCase "returns an empty list when no shares exist" $ do
      given $ do
        _ <- insertVm "bare" VmStopped
        pure ()
      when_ $ whenSharedDirList 1
      then_ $ responseIs $ \case
        RespSharedDirList [] -> True
        _ -> False

    testCase "returns each inserted shared dir" $ do
      given $ do
        vmId <- insertVm "v" VmStopped
        _ <- insertSharedDir vmId "/host/a" "tag-a" CacheAuto False
        _ <- insertSharedDir vmId "/host/b" "tag-b" CacheAuto True
        pure ()
      when_ $ whenSharedDirList 1
      then_ $ responseIs $ \case
        RespSharedDirList xs -> length xs == 2
        _ -> False

  describe "whenSharedDirAdd" $ do
    testCase "returns VmNotFound for an unknown VM" $ do
      when_ $ whenSharedDirAdd 999 "/host" "tag" CacheAuto False
      then_ responseIsVmNotFound

    testCase "rejects an all-digit tag (ambiguous with numeric ids)" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        pure ()
      -- `validateName` (Corvus.Handlers.Resolve) refuses all-digit
      -- names because they collide with numeric `EntityRef`s.
      when_ $ whenSharedDirAdd 1 "/host" "42" CacheAuto False
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

    testCase "writes a row when VM exists and tag is fresh" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        pure ()
      when_ $ whenSharedDirAdd 1 "/host/data" "data" CacheAuto False
      then_ $ responseIs $ \case
        RespSharedDirAdded _ -> True
        _ -> False

    testCase "refuses a duplicate tag for the same VM" $ do
      given $ do
        vmId <- insertVm "v" VmStopped
        _ <- insertSharedDir vmId "/host/x" "shared" CacheAuto False
        pure ()
      when_ $ whenSharedDirAdd 1 "/host/y" "shared" CacheAuto False
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

  describe "whenSharedDirRemove" $ do
    testCase "returns SharedDirNotFound for an unknown share" $ do
      given $ do
        _ <- insertVm "v" VmStopped
        pure ()
      when_ $ whenSharedDirRemove 1 999
      then_ $ responseIs $ \case
        RespSharedDirNotFound -> True
        _ -> False

    testCase "removes the row when it exists" $ do
      given $ do
        vmId <- insertVm "v" VmStopped
        _ <- insertSharedDir vmId "/host/x" "tag" CacheAuto False
        pure ()
      when_ $ whenSharedDirRemove 1 1
      then_ $ responseIs (== RespSharedDirOk)
      when_ $ whenSharedDirList 1
      then_ $ responseIs $ \case
        RespSharedDirList [] -> True
        _ -> False
