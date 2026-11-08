{-# LANGUAGE OverloadedStrings #-}

-- | Bake VM teardown for the build pipeline.
--
-- Safely stops and deletes the bake VM.
module Corvus.Handlers.Build.CleanupBakeVm
  ( cleanupBakeVm
  )
where

import Corvus.Action
import Corvus.Handlers.Vm (VmDelete (..), VmStop (..))
import Corvus.Model (TaskSubsystem (..), VmStatus (..))
import qualified Corvus.Model as M
import Corvus.Protocol (Response (..))
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.Word (Word32)
import Database.Persist (get)
import Database.Persist.Sql (runSqlPool, toSqlKey)

-- | Tear down the bake VM safely.
--
-- Stops the VM and runs @VmDelete keepDisks=False@. The delete reaps
-- every disk still attached with @DiskImage.ephemeral=True@ — which
-- covers every disk the build pipeline creates: the template-
-- instantiated overlay/clone/create-strategy disks, the bake
-- artifact disk (the published artifact is a clone, see
-- 'publishArtifact') and any cloud-init ISO.
-- Shared infrastructure (direct-strategy template disks, registered
-- base images) is created with @ephemeral=False@ and is preserved.
cleanupBakeVm :: ActionContext -> Int64 -> IO ()
cleanupBakeVm context vmIdLong = do
  cancelled <- isCancelled context
  let ctx = context {acIgnoreCancellation = True}
      state = acState ctx
      prepare = do
        let deletion = VmDelete vmIdLong False False
        disks <- actionResources state deletion
        vm <- actionObjectResources state deletion
        -- Reuse deletion's access modes: shared firmware must not upgrade
        -- the build's inherited shared lease, while ephemeral disks need
        -- exclusive access before they can be reaped.
        pure ((), disks ++ vm)
  withPreparedOperationLeases ctx prepare $ \locked () -> do
    existing <- runSqlPool (get (toSqlKey vmIdLong)) (ssDbPool state)
    case existing of
      Nothing -> pure ()
      Just vm -> do
        if M.vmStatus vm `elem` [VmStopped, VmSaved, VmError]
          then pure ()
          else do
            stopped <- runActionAsSubtask locked (StopBakeVm vmIdLong (if cancelled then 0 else 300))
            case stopped of
              RespVmStateChanged VmStopped -> pure ()
              _ -> ioError (userError ("stop bake VM: " ++ show stopped))
        result <- runActionAsSubtask locked (VmDelete vmIdLong False False)
        case result of
          RespVmDeleted -> pure ()
          _ -> ioError (userError ("delete bake VM: " ++ show result))

-- A guest can shut itself down between inspection and stop dispatch. Keep
-- public VmStop strict while making this private cleanup action idempotent.
data StopBakeVm = StopBakeVm Int64 Word32

instance Action StopBakeVm where
  actionSubsystem _ = SubVm
  actionCommand _ = "stop"
  actionEntityId (StopBakeVm ident _) = Just (fromIntegral ident)
  actionExecute ctx (StopBakeVm ident timeout) = do
    result <- actionExecute ctx (VmStop ident timeout True)
    pure $ case result of
      RespInvalidTransition VmStopped _ -> RespVmStateChanged VmStopped
      _ -> result
