{-# LANGUAGE OverloadedStrings #-}

-- | Bake VM teardown for the build pipeline.
--
-- Safely stops and deletes the bake VM.
module Corvus.Handlers.Build.CleanupBakeVm
  ( cleanupBakeVm
  )
where

import Corvus.Action (mkActionContext, runActionAsSubtask)
import Corvus.Handlers.Vm (VmDelete (..), VmStop (..))
import Corvus.Model (TaskId)
import Corvus.Types (ServerState)
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
cleanupBakeVm :: ServerState -> TaskId -> Int64 -> IO ()
cleanupBakeVm state parentTaskId vmIdLong = do
  -- Best-effort stop first; VmDelete refuses while running.
  _ <- runActionAsSubtask (mkActionContext state parentTaskId "system") (VmStop vmIdLong 300)
  _ <- runActionAsSubtask (mkActionContext state parentTaskId "system") (VmDelete vmIdLong False False)
  pure ()
