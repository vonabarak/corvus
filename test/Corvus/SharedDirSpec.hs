{-# LANGUAGE OverloadedStrings #-}

-- | Tests for shared directory management.
-- These tests verify the daemon's shared directory handlers work correctly.
module Corvus.SharedDirSpec (spec) where

import Corvus.Model (SharedDirCache (..))
import Corvus.Protocol
import Test.DSL.Given
import Test.DSL.When
import Test.Hspec
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "shared directory list" $ do
    testCase "returns empty list for VM with no shared dirs" $ do
      vmId <- givenVmExists "test-vm"
      result <- whenSharedDirList vmId
      thenSharedDirListIsEmpty result

    testCase "returns all shared directories for VM" $ do
      vmId <- givenVmExists "test-vm"
      _ <- givenSharedDirExists vmId "/host/path1" "share1"
      _ <- givenSharedDirExists vmId "/host/path2" "share2"
      result <- whenSharedDirList vmId
      thenSharedDirListHasCount result 2

    testCase "returns not found for non-existent VM" $ do
      result <- whenSharedDirList 999
      thenVmNotFound result

  describe "shared directory add" $ do
    testCase "adds shared directory to stopped VM" $ do
      vmId <- givenVmExists "test-vm"
      result <- whenSharedDirAdd vmId "/host/share" "myshare" CacheAuto False
      thenSharedDirAdded result

    testCase "fails for non-existent VM" $ do
      result <- whenSharedDirAdd 999 "/host/share" "myshare" CacheAuto False
      thenVmNotFound result

    testCase "fails for duplicate tag" $ do
      vmId <- givenVmExists "test-vm"
      _ <- whenSharedDirAdd vmId "/host/share1" "mytag" CacheAuto False
      result <- whenSharedDirAdd vmId "/host/share2" "mytag" CacheAuto False
      thenSharedDirError result "already exists"

  describe "shared directory remove" $ do
    testCase "removes shared directory from stopped VM" $ do
      vmId <- givenVmExists "test-vm"
      dirId <- givenSharedDirExists vmId "/host/share" "myshare"
      result <- whenSharedDirRemove vmId dirId
      thenSharedDirOk result

    testCase "fails for non-existent VM" $ do
      result <- whenSharedDirRemove 999 1
      thenVmNotFound result

    testCase "fails for non-existent shared dir" $ do
      vmId <- givenVmExists "test-vm"
      result <- whenSharedDirRemove vmId 999
      thenSharedDirNotFound result
