-- | Request handling module.
-- Re-exports handlers from submodules and provides the main dispatch function.
module Corvus.Handlers
  ( -- * Request handling
    handleRequest

    -- * Re-exports from submodules
  , module Corvus.Handlers.Core
  , module Corvus.Handlers.Vm
  , module Corvus.Handlers.Disk
  , module Corvus.Handlers.SharedDir
  , module Corvus.Handlers.NetIf
  , module Corvus.Handlers.SshKey
  , module Corvus.Handlers.Template
  , module Corvus.Handlers.Network
  )
where

import Corvus.Handlers.Core
import Corvus.Handlers.Disk
import Corvus.Handlers.NetIf
import Corvus.Handlers.Network
import Corvus.Handlers.SharedDir
import Corvus.Handlers.SshKey
import Corvus.Handlers.Template
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
  ReqVmCreate name cpus ram desc headless -> handleVmCreate state name cpus ram desc headless
  ReqVmDelete vmId -> handleVmDelete state vmId
  ReqVmStart vmId -> handleVmStart state vmId
  ReqVmStop vmId -> handleVmStop state vmId
  ReqVmPause vmId -> handleVmPause state vmId
  ReqVmReset vmId -> handleVmReset state vmId
  ReqVmEdit vmId mCpus mRam mDesc mHeadless -> handleVmEdit state vmId mCpus mRam mDesc mHeadless
  -- Disk image handlers
  ReqDiskCreate name format sizeMb -> handleDiskCreate state name format sizeMb
  ReqDiskCreateOverlay name baseDiskId -> handleDiskCreateOverlay state name baseDiskId
  ReqDiskRegister name path format sizeMb -> handleDiskRegister state name path format sizeMb
  ReqDiskDelete diskId -> handleDiskDelete state diskId
  ReqDiskResize diskId newSizeMb -> handleDiskResize state diskId newSizeMb
  ReqDiskList -> handleDiskList state
  ReqDiskShow diskId -> handleDiskShow state diskId
  ReqDiskClone name baseDiskId optionalPath -> handleDiskClone state name baseDiskId optionalPath
  -- Snapshot handlers
  ReqSnapshotCreate diskId name -> handleSnapshotCreate state diskId name
  ReqSnapshotDelete diskId snapshotId -> handleSnapshotDelete state diskId snapshotId
  ReqSnapshotRollback diskId snapshotId -> handleSnapshotRollback state diskId snapshotId
  ReqSnapshotMerge diskId snapshotId -> handleSnapshotMerge state diskId snapshotId
  ReqSnapshotList diskId -> handleSnapshotList state diskId
  -- Attach/detach handlers
  ReqDiskAttach vmId diskId interface media readOnly discard cache -> handleDiskAttach state vmId diskId interface media readOnly discard cache
  ReqDiskDetach vmId driveId -> handleDiskDetach state vmId driveId
  -- Shared directory handlers
  ReqSharedDirAdd vmId path tag cache readOnly -> handleSharedDirAdd state vmId path tag cache readOnly
  ReqSharedDirRemove vmId sharedDirId -> handleSharedDirRemove state vmId sharedDirId
  ReqSharedDirList vmId -> handleSharedDirList state vmId
  -- Network interface handlers
  ReqNetIfAdd vmId ifaceType hostDev mac nwId -> handleNetIfAdd state vmId ifaceType hostDev mac nwId
  ReqNetIfRemove vmId netIfId -> handleNetIfRemove state vmId netIfId
  ReqNetIfList vmId -> handleNetIfList state vmId
  -- SSH key handlers
  ReqSshKeyCreate name publicKey -> handleSshKeyCreate state name publicKey
  ReqSshKeyDelete keyId -> handleSshKeyDelete state keyId
  ReqSshKeyList -> handleSshKeyList state
  ReqSshKeyAttach vmId keyId -> handleSshKeyAttach state vmId keyId
  ReqSshKeyDetach vmId keyId -> handleSshKeyDetach state vmId keyId
  ReqSshKeyListForVm vmId -> handleSshKeyListForVm state vmId
  -- Template handlers
  ReqTemplateCreate yaml -> handleTemplateCreate state yaml
  ReqTemplateDelete tid -> handleTemplateDelete state tid
  ReqTemplateList -> handleTemplateList state
  ReqTemplateShow tid -> handleTemplateShow state tid
  ReqTemplateInstantiate tid name -> handleTemplateInstantiate state tid name
  -- Network handlers
  ReqNetworkCreate name -> handleNetworkCreate state name
  ReqNetworkDelete nwId -> handleNetworkDelete state nwId
  ReqNetworkStart nwId -> handleNetworkStart state nwId
  ReqNetworkStop nwId force -> handleNetworkStop state nwId force
  ReqNetworkList -> handleNetworkList state
  ReqNetworkShow nwId -> handleNetworkShow state nwId
