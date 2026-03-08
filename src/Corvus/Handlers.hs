-- | Request handling module.
-- Re-exports handlers from submodules and provides the main dispatch function.
module Corvus.Handlers
  ( -- * Request handling
    handleRequest,

    -- * Re-exports from submodules
    module Corvus.Handlers.Core,
    module Corvus.Handlers.Vm,
    module Corvus.Handlers.Disk,
  )
where

import Corvus.Handlers.Core
import Corvus.Handlers.Disk
import Corvus.Handlers.Vm
import Corvus.Protocol
import Corvus.Types

--------------------------------------------------------------------------------
-- Request Dispatch
--------------------------------------------------------------------------------

-- | Handle a request and produce a response
handleRequest :: ServerState -> Request -> IO Response
handleRequest state req = case req of
  -- Core handlers
  ReqPing -> handlePing
  ReqStatus -> handleStatus state
  ReqShutdown -> handleShutdown state
  -- VM handlers
  ReqListVms -> handleVmList state
  ReqShowVm vmId -> handleVmShow state vmId
  ReqVmStart vmId -> handleVmStart state vmId
  ReqVmStop vmId -> handleVmStop state vmId
  ReqVmPause vmId -> handleVmPause state vmId
  ReqVmReset vmId -> handleVmReset state vmId
  -- Disk image handlers
  ReqDiskCreate name format sizeMb -> handleDiskCreate state name format sizeMb
  ReqDiskDelete diskId -> handleDiskDelete state diskId
  ReqDiskResize diskId newSizeMb -> handleDiskResize state diskId newSizeMb
  ReqDiskList -> handleDiskList state
  ReqDiskShow diskId -> handleDiskShow state diskId
  -- Snapshot handlers
  ReqSnapshotCreate diskId name -> handleSnapshotCreate state diskId name
  ReqSnapshotDelete diskId snapshotId -> handleSnapshotDelete state diskId snapshotId
  ReqSnapshotRollback diskId snapshotId -> handleSnapshotRollback state diskId snapshotId
  ReqSnapshotMerge diskId snapshotId -> handleSnapshotMerge state diskId snapshotId
  ReqSnapshotList diskId -> handleSnapshotList state diskId
  -- Attach/detach handlers
  ReqDiskAttach vmId diskId interface media -> handleDiskAttach state vmId diskId interface media
  ReqDiskDetach vmId driveId -> handleDiskDetach state vmId driveId
