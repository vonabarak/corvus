{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.OverlayExtraSpec (spec) where

import Corvus.Protocol (Response (..))
import qualified Data.Text as T
import Test.Prelude

spec :: Spec
spec = withTestDb $ do
  describe "overlay extra protections" $ do
    testCase "prevent attaching base image read-write when overlays exist" $ do
      given $ do
        _ <- insertVm "vm1" VmStopped
        _ <- insertDiskImage "base-disk" "base.qcow2" FormatQcow2
        _ <- insertDiskImageWithBacking "overlay-disk" "overlay.qcow2" FormatQcow2 Nothing (Just 1)
        pure ()
      -- Try to attach base-disk (ID 1) to vm1 (ID 1) read-write
      when_ $ diskAttach 1 1 InterfaceVirtio (Just MediaDisk)
      then_ responseIsDiskHasOverlays

    testCase "allow attaching base image read-only when overlays exist" $ do
      given $ do
        _ <- insertVm "vm1" VmStopped
        _ <- insertDiskImage "base-disk" "base.qcow2" FormatQcow2
        _ <- insertDiskImageWithBacking "overlay-disk" "overlay.qcow2" FormatQcow2 Nothing (Just 1)
        pure ()
      -- Try to attach base-disk (ID 1) to vm1 (ID 1) read-only
      when_ $ diskAttachReadOnly 1 1 InterfaceVirtio (Just MediaDisk)
      then_ responseIsSuccess
      then_ $ driveExistsForVm 1 1

    testCase "prevent resizing base image when overlays exist" $ do
      given $ do
        _ <- insertDiskImage "base-disk" "base.qcow2" FormatQcow2
        _ <- insertDiskImageWithBacking "overlay-disk" "overlay.qcow2" FormatQcow2 Nothing (Just 1)
        pure ()
      -- Try to resize base-disk (ID 1)
      when_ $ diskResize 1 20480
      then_ responseIsDiskHasOverlays
