{-# LANGUAGE OverloadedStrings #-}

-- | Bake VM teardown for the build pipeline.
--
-- Safely stops and deletes the bake VM, with cache-aware logic to
-- preserve it when @--build-cache@ rows reference it.
module Corvus.Handlers.Build.CleanupBakeVm
  ( cleanupBakeVm
  )
where

import Corvus.Action (mkActionContext, runActionAsSubtask)
import qualified Corvus.Build.Cache.Store as CStore
import Corvus.Handlers.Vm (VmDelete (..), VmStop (..))
import Corvus.Model (TaskId)
import Corvus.Types (ServerState)
import Data.Int (Int64)
import Database.Persist.Sql (fromSqlKey)

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
--
-- When the bake VM has any 'BuildCacheEntry' rows referencing it
-- (@--build-cache@ on the build YAML or the @--build-cache@ CLI
-- flag was set on a prior run), the @VmDelete@ is skipped: the VM
-- + its ephemeral disks survive so future @--use-cache@ builds can
-- roll back to a cached step. Only an explicit @crv vm delete@
-- reaps a cache-retained bake VM (which cascades the cache rows
-- via 'deleteVm').
cleanupBakeVm :: ServerState -> TaskId -> Int64 -> IO ()
cleanupBakeVm state parentTaskId vmIdLong = do
  -- Best-effort stop first; VmDelete refuses while running.
  _ <- runActionAsSubtask (mkActionContext state parentTaskId "system") (VmStop vmIdLong 300)
  -- If any 'BuildCacheEntry' row still references this bake VM, keep
  -- it alive: future builds with @--use-cache@ need its disks intact
  -- to roll back to a cached step. Drop the cache rows (via VM
  -- delete) only when the build's own cleanup or a subsequent
  -- @crv vm delete@ removes the VM explicitly.
  hasCache <- CStore.bakeVmHasCache state vmIdLong
  if hasCache
    then pure ()
    else do
      _ <- runActionAsSubtask (mkActionContext state parentTaskId "system") (VmDelete vmIdLong False False)
      pure ()
