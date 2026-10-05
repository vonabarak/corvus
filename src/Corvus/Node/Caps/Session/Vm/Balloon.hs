{-# LANGUAGE OverloadedStrings #-}

-- | Node-local balloon adjustment through QMP.
module Corvus.Node.Caps.Session.Vm.Balloon (handleVmSetBalloon) where

import qualified Capnp.Gen.Nodeagent as CGNA
import Control.Concurrent.STM (atomically, readTVarIO)
import Corvus.Node.Caps.Session.Utils (SessionCap (..), agentQemuConfig)
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.Qmp as Qmp
import qualified Corvus.Node.VmSpec as VS
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Word (Word64)

handleVmSetBalloon
  :: SessionCap -> Int64 -> Word64 -> IO (CGNA.Parsed CGNA.Session'vmSetBalloon'results)
handleVmSetBalloon sc vmId target = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> pure $ reply CGNA.BalloonStatus'notRunning "VM is not running"
    Just live -> do
      exited <- readTVarIO (L.vlsLastExitCode live)
      let spec = L.vlsSpec live
      if isJust exited
        then pure $ reply CGNA.BalloonStatus'notRunning "VM has exited"
        else
          if not (VS.vsBalloon spec)
            then pure $ reply CGNA.BalloonStatus'deviceNotEnabled "VM has no VirtIO balloon device"
            else
              if target == 0 || toInteger target > toInteger (VS.vsRamMb spec) * 1024 * 1024
                then pure $ reply CGNA.BalloonStatus'invalidTarget "Target must be positive and not exceed the live RAM ceiling"
                else do
                  result <- Qmp.qmpSetBalloon agentQemuConfig vmId target
                  pure $ case result of
                    Right () -> reply CGNA.BalloonStatus'success ""
                    Left Qmp.QmpBalloonDriverNotReady -> reply CGNA.BalloonStatus'driverNotReady "VirtIO balloon guest driver is not ready"
                    Left (Qmp.QmpBalloonError err) -> reply CGNA.BalloonStatus'failed err
  where
    reply = CGNA.Session'vmSetBalloon'results
