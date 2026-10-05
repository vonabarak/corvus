{-# LANGUAGE OverloadedStrings #-}

-- | Manual adjustment of a running VM's guest memory target.
module Corvus.Handlers.Vm.Balloon (VmSetBalloon (..)) where

import qualified Capnp.Gen.Nodeagent as CGNA
import Corvus.Action (Action (..), ActionContext (..))
import Corvus.Model (TaskSubsystem (..), Vm (..), VmStatus (..))
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol (Response (..))
import Corvus.Types (ssDbPool)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Word (Word64)
import Database.Persist (get)
import Database.Persist.Sql (runSqlPool, toSqlKey)

data VmSetBalloon = VmSetBalloon
  { vsbVmId :: !Int64
  , vsbTargetBytes :: !Word64
  }

instance Action VmSetBalloon where
  actionSubsystem _ = SubVm
  actionCommand _ = "balloon"
  actionEntityId = Just . fromIntegral . vsbVmId
  actionExecute ctx a = do
    let state = acState ctx
        vmId = vsbVmId a
        target = vsbTargetBytes a
    mVm <- runSqlPool (get (toSqlKey vmId)) (ssDbPool state)
    case mVm of
      Nothing -> pure RespVmNotFound
      Just vm
        | vmStatus vm /= VmRunning -> pure RespVmNotRunning
        | not (vmBalloon vm) -> pure RespBalloonDeviceNotEnabled
        | target == 0 || toInteger target > toInteger (vmRamMb vm) * 1024 * 1024 ->
            pure RespInvalidBalloonTarget
        | otherwise -> do
            result <- withVmNodeAgent state vmId $ \nac -> NOA.vmSetBalloon nac vmId target
            pure $ case result of
              Left err -> RespBalloonError ("nodeagent unavailable: " <> err)
              Right (Left err) -> RespBalloonError ("vmSetBalloon: " <> T.pack (show err))
              Right (Right (status, message)) -> case status of
                CGNA.BalloonStatus'success -> RespOk
                CGNA.BalloonStatus'notRunning -> RespVmNotRunning
                CGNA.BalloonStatus'deviceNotEnabled -> RespBalloonDeviceNotEnabled
                CGNA.BalloonStatus'driverNotReady -> RespBalloonDriverNotReady
                CGNA.BalloonStatus'invalidTarget -> RespInvalidBalloonTarget
                _ -> RespBalloonError message
