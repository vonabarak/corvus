-- | Bake VM teardown for the build pipeline.
--
-- Safely stops and deletes the bake VM.
module Corvus.Handlers.Build.CleanupBakeVm
  ( cleanupBakeVm
  )
where

import Corvus.Action (ActionContext (..), isCancelled, runActionAsSubtask)
import Corvus.Handlers.Vm (VmDelete (..), VmStop (..))
import Corvus.Protocol (Response (..))
import Data.Int (Int64)

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
  -- A cancelled provisioner can still hold the guest-agent connection.
  -- Hard-stop cancelled builds so cleanup cannot wait behind that command.
  -- VmDelete refuses while running.
  _ <- runActionAsSubtask ctx (VmStop vmIdLong (if cancelled then 0 else 300) True)
  result <- runActionAsSubtask ctx (VmDelete vmIdLong False False)
  case result of
    RespVmDeleted -> pure ()
    _ -> ioError (userError ("delete bake VM: " ++ show result))
