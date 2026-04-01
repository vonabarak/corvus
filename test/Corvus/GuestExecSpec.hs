{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.GuestExecSpec (spec) where

import Corvus.Protocol (Ref (..), Request (..), Response (..))
import qualified Data.Text as T
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "guest-exec" $ do
    testCase "fails for non-existent VM" $ do
      _ <- executeRequest (ReqGuestExec (Ref "999") "echo hello")
      then_ $ responseIs $ \case
        RespVmNotFound -> True
        _ -> False

    testCase "fails for stopped VM" $ do
      vmId <- given $ insertVm "stopped-vm" VmStopped
      _ <- executeRequest (ReqGuestExec (Ref (T.pack (show vmId))) "echo hello")
      then_ $ responseIs $ \case
        RespInvalidTransition _ _ -> True
        _ -> False

    testCase "fails for VM without guest agent" $ do
      -- VM is running but guest agent not enabled
      vmId <- given $ insertVm "no-agent-vm" VmRunning
      _ <- executeRequest (ReqGuestExec (Ref (T.pack (show vmId))) "echo hello")
      then_ $ responseIs $ \case
        RespGuestAgentNotEnabled -> True
        _ -> False
