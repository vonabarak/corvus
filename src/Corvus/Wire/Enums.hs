{-# LANGUAGE LambdaCase #-}

-- | Bidirectional conversion between Corvus's persistent-entity enums
-- (defined in 'Corvus.Model') and their Cap'n Proto counterparts
-- (defined in @schema/enums.capnp@, exposed via @Capnp.Gen.Enums@).
--
-- Encoding (Haskell → Cap'n Proto) is total. Decoding can fail if the
-- peer sent an enum tag this schema version does not know about — those
-- cases return 'WireUnknownEnum'.
module Corvus.Wire.Enums
  ( -- * VmStatus
    toCapnpVmStatus
  , fromCapnpVmStatus

    -- * DriveInterface
  , toCapnpDriveInterface
  , fromCapnpDriveInterface

    -- * DriveFormat
  , toCapnpDriveFormat
  , fromCapnpDriveFormat

    -- * DriveMedia
  , toCapnpDriveMedia
  , fromCapnpDriveMedia

    -- * CacheType
  , toCapnpCacheType
  , fromCapnpCacheType

    -- * NetInterfaceType
  , toCapnpNetInterfaceType
  , fromCapnpNetInterfaceType

    -- * SharedDirCache
  , toCapnpSharedDirCache
  , fromCapnpSharedDirCache

    -- * TemplateCloneStrategy
  , toCapnpTemplateCloneStrategy
  , fromCapnpTemplateCloneStrategy

    -- * TaskSubsystem
  , toCapnpTaskSubsystem
  , fromCapnpTaskSubsystem

    -- * TaskResult
  , toCapnpTaskResult
  , fromCapnpTaskResult

    -- * NodeAdminState
  , toCapnpNodeAdminState
  , fromCapnpNodeAdminState
  )
where

import qualified Capnp.Gen.Enums as CGE
import qualified Corvus.Model as M
import Corvus.Wire.Errors (WireError (..))

-- ---------------------------------------------------------------------
-- VmStatus
-- ---------------------------------------------------------------------

toCapnpVmStatus :: M.VmStatus -> CGE.VmStatus
toCapnpVmStatus = \case
  M.VmStopped -> CGE.VmStatus'stopped
  M.VmStarting -> CGE.VmStatus'starting
  M.VmRunning -> CGE.VmStatus'running
  M.VmStopping -> CGE.VmStatus'stopping
  M.VmPaused -> CGE.VmStatus'paused
  M.VmSaved -> CGE.VmStatus'saved
  M.VmError -> CGE.VmStatus'error

fromCapnpVmStatus :: CGE.VmStatus -> Either WireError M.VmStatus
fromCapnpVmStatus = \case
  CGE.VmStatus'stopped -> Right M.VmStopped
  CGE.VmStatus'starting -> Right M.VmStarting
  CGE.VmStatus'running -> Right M.VmRunning
  CGE.VmStatus'stopping -> Right M.VmStopping
  CGE.VmStatus'paused -> Right M.VmPaused
  CGE.VmStatus'saved -> Right M.VmSaved
  CGE.VmStatus'error -> Right M.VmError
  CGE.VmStatus'unknown' n -> Left (WireUnknownEnum "VmStatus" n)

-- ---------------------------------------------------------------------
-- DriveInterface
-- ---------------------------------------------------------------------

toCapnpDriveInterface :: M.DriveInterface -> CGE.DriveInterface
toCapnpDriveInterface = \case
  M.InterfaceVirtio -> CGE.DriveInterface'virtio
  M.InterfaceIde -> CGE.DriveInterface'ide
  M.InterfaceScsi -> CGE.DriveInterface'scsi
  M.InterfaceSata -> CGE.DriveInterface'sata
  M.InterfaceNvme -> CGE.DriveInterface'nvme
  M.InterfacePflash -> CGE.DriveInterface'pflash
  M.InterfaceFloppy -> CGE.DriveInterface'floppy

fromCapnpDriveInterface :: CGE.DriveInterface -> Either WireError M.DriveInterface
fromCapnpDriveInterface = \case
  CGE.DriveInterface'virtio -> Right M.InterfaceVirtio
  CGE.DriveInterface'ide -> Right M.InterfaceIde
  CGE.DriveInterface'scsi -> Right M.InterfaceScsi
  CGE.DriveInterface'sata -> Right M.InterfaceSata
  CGE.DriveInterface'nvme -> Right M.InterfaceNvme
  CGE.DriveInterface'pflash -> Right M.InterfacePflash
  CGE.DriveInterface'floppy -> Right M.InterfaceFloppy
  CGE.DriveInterface'unknown' n -> Left (WireUnknownEnum "DriveInterface" n)

-- ---------------------------------------------------------------------
-- DriveFormat
-- ---------------------------------------------------------------------

toCapnpDriveFormat :: M.DriveFormat -> CGE.DriveFormat
toCapnpDriveFormat = \case
  M.FormatQcow2 -> CGE.DriveFormat'qcow2
  M.FormatRaw -> CGE.DriveFormat'raw
  M.FormatVmdk -> CGE.DriveFormat'vmdk
  M.FormatVdi -> CGE.DriveFormat'vdi
  M.FormatVpc -> CGE.DriveFormat'vpc
  M.FormatVhdx -> CGE.DriveFormat'vhdx

fromCapnpDriveFormat :: CGE.DriveFormat -> Either WireError M.DriveFormat
fromCapnpDriveFormat = \case
  CGE.DriveFormat'qcow2 -> Right M.FormatQcow2
  CGE.DriveFormat'raw -> Right M.FormatRaw
  CGE.DriveFormat'vmdk -> Right M.FormatVmdk
  CGE.DriveFormat'vdi -> Right M.FormatVdi
  CGE.DriveFormat'vpc -> Right M.FormatVpc
  CGE.DriveFormat'vhdx -> Right M.FormatVhdx
  CGE.DriveFormat'unknown' n -> Left (WireUnknownEnum "DriveFormat" n)

-- ---------------------------------------------------------------------
-- DriveMedia
-- ---------------------------------------------------------------------

toCapnpDriveMedia :: M.DriveMedia -> CGE.DriveMedia
toCapnpDriveMedia = \case
  M.MediaDisk -> CGE.DriveMedia'disk
  M.MediaCdrom -> CGE.DriveMedia'cdrom

fromCapnpDriveMedia :: CGE.DriveMedia -> Either WireError M.DriveMedia
fromCapnpDriveMedia = \case
  CGE.DriveMedia'disk -> Right M.MediaDisk
  CGE.DriveMedia'cdrom -> Right M.MediaCdrom
  CGE.DriveMedia'unknown' n -> Left (WireUnknownEnum "DriveMedia" n)

-- ---------------------------------------------------------------------
-- CacheType
-- ---------------------------------------------------------------------

toCapnpCacheType :: M.CacheType -> CGE.CacheType
toCapnpCacheType = \case
  M.CacheNone -> CGE.CacheType'none
  M.CacheWriteback -> CGE.CacheType'writeback
  M.CacheWritethrough -> CGE.CacheType'writethrough
  M.CacheDirectsync -> CGE.CacheType'directsync
  M.CacheUnsafe -> CGE.CacheType'unsafe

fromCapnpCacheType :: CGE.CacheType -> Either WireError M.CacheType
fromCapnpCacheType = \case
  CGE.CacheType'none -> Right M.CacheNone
  CGE.CacheType'writeback -> Right M.CacheWriteback
  CGE.CacheType'writethrough -> Right M.CacheWritethrough
  CGE.CacheType'directsync -> Right M.CacheDirectsync
  CGE.CacheType'unsafe -> Right M.CacheUnsafe
  CGE.CacheType'unknown' n -> Left (WireUnknownEnum "CacheType" n)

-- ---------------------------------------------------------------------
-- NetInterfaceType
-- ---------------------------------------------------------------------

toCapnpNetInterfaceType :: M.NetInterfaceType -> CGE.NetInterfaceType
toCapnpNetInterfaceType = \case
  M.NetUser -> CGE.NetInterfaceType'user
  M.NetTap -> CGE.NetInterfaceType'tap
  M.NetBridge -> CGE.NetInterfaceType'bridge
  M.NetMacvtap -> CGE.NetInterfaceType'macvtap
  M.NetVde -> CGE.NetInterfaceType'vde
  M.NetManaged -> CGE.NetInterfaceType'managed

fromCapnpNetInterfaceType :: CGE.NetInterfaceType -> Either WireError M.NetInterfaceType
fromCapnpNetInterfaceType = \case
  CGE.NetInterfaceType'user -> Right M.NetUser
  CGE.NetInterfaceType'tap -> Right M.NetTap
  CGE.NetInterfaceType'bridge -> Right M.NetBridge
  CGE.NetInterfaceType'macvtap -> Right M.NetMacvtap
  CGE.NetInterfaceType'vde -> Right M.NetVde
  CGE.NetInterfaceType'managed -> Right M.NetManaged
  CGE.NetInterfaceType'unknown' n -> Left (WireUnknownEnum "NetInterfaceType" n)

-- ---------------------------------------------------------------------
-- SharedDirCache
-- ---------------------------------------------------------------------

toCapnpSharedDirCache :: M.SharedDirCache -> CGE.SharedDirCache
toCapnpSharedDirCache = \case
  M.CacheAlways -> CGE.SharedDirCache'always
  M.CacheAuto -> CGE.SharedDirCache'auto
  M.CacheNever -> CGE.SharedDirCache'never

fromCapnpSharedDirCache :: CGE.SharedDirCache -> Either WireError M.SharedDirCache
fromCapnpSharedDirCache = \case
  CGE.SharedDirCache'always -> Right M.CacheAlways
  CGE.SharedDirCache'auto -> Right M.CacheAuto
  CGE.SharedDirCache'never -> Right M.CacheNever
  CGE.SharedDirCache'unknown' n -> Left (WireUnknownEnum "SharedDirCache" n)

-- ---------------------------------------------------------------------
-- TemplateCloneStrategy
-- ---------------------------------------------------------------------

toCapnpTemplateCloneStrategy :: M.TemplateCloneStrategy -> CGE.TemplateCloneStrategy
toCapnpTemplateCloneStrategy = \case
  M.StrategyClone -> CGE.TemplateCloneStrategy'clone
  M.StrategyOverlay -> CGE.TemplateCloneStrategy'overlay
  M.StrategyDirect -> CGE.TemplateCloneStrategy'direct
  M.StrategyCreate -> CGE.TemplateCloneStrategy'create

fromCapnpTemplateCloneStrategy :: CGE.TemplateCloneStrategy -> Either WireError M.TemplateCloneStrategy
fromCapnpTemplateCloneStrategy = \case
  CGE.TemplateCloneStrategy'clone -> Right M.StrategyClone
  CGE.TemplateCloneStrategy'overlay -> Right M.StrategyOverlay
  CGE.TemplateCloneStrategy'direct -> Right M.StrategyDirect
  CGE.TemplateCloneStrategy'create -> Right M.StrategyCreate
  CGE.TemplateCloneStrategy'unknown' n -> Left (WireUnknownEnum "TemplateCloneStrategy" n)

-- ---------------------------------------------------------------------
-- TaskSubsystem
-- ---------------------------------------------------------------------

toCapnpTaskSubsystem :: M.TaskSubsystem -> CGE.TaskSubsystem
toCapnpTaskSubsystem = \case
  M.SubVm -> CGE.TaskSubsystem'vm
  M.SubDisk -> CGE.TaskSubsystem'disk
  M.SubNetwork -> CGE.TaskSubsystem'network
  M.SubSshKey -> CGE.TaskSubsystem'sshKey
  M.SubTemplate -> CGE.TaskSubsystem'template
  M.SubSharedDir -> CGE.TaskSubsystem'sharedDir
  M.SubSnapshot -> CGE.TaskSubsystem'snapshot
  M.SubSystem -> CGE.TaskSubsystem'system
  M.SubApply -> CGE.TaskSubsystem'apply
  M.SubBuild -> CGE.TaskSubsystem'build
  M.SubNode -> CGE.TaskSubsystem'node
  M.SubMigration -> CGE.TaskSubsystem'migration

fromCapnpTaskSubsystem :: CGE.TaskSubsystem -> Either WireError M.TaskSubsystem
fromCapnpTaskSubsystem = \case
  CGE.TaskSubsystem'vm -> Right M.SubVm
  CGE.TaskSubsystem'disk -> Right M.SubDisk
  CGE.TaskSubsystem'network -> Right M.SubNetwork
  CGE.TaskSubsystem'sshKey -> Right M.SubSshKey
  CGE.TaskSubsystem'template -> Right M.SubTemplate
  CGE.TaskSubsystem'sharedDir -> Right M.SubSharedDir
  CGE.TaskSubsystem'snapshot -> Right M.SubSnapshot
  CGE.TaskSubsystem'system -> Right M.SubSystem
  CGE.TaskSubsystem'apply -> Right M.SubApply
  CGE.TaskSubsystem'build -> Right M.SubBuild
  CGE.TaskSubsystem'node -> Right M.SubNode
  CGE.TaskSubsystem'migration -> Right M.SubMigration
  CGE.TaskSubsystem'unknown' n -> Left (WireUnknownEnum "TaskSubsystem" n)

-- ---------------------------------------------------------------------
-- TaskResult
-- ---------------------------------------------------------------------

toCapnpTaskResult :: M.TaskResult -> CGE.TaskResult
toCapnpTaskResult = \case
  M.TaskRunning -> CGE.TaskResult'running
  M.TaskSuccess -> CGE.TaskResult'success
  M.TaskError -> CGE.TaskResult'error
  M.TaskNotStarted -> CGE.TaskResult'notStarted
  M.TaskCancelled -> CGE.TaskResult'cancelled

fromCapnpTaskResult :: CGE.TaskResult -> Either WireError M.TaskResult
fromCapnpTaskResult = \case
  CGE.TaskResult'running -> Right M.TaskRunning
  CGE.TaskResult'success -> Right M.TaskSuccess
  CGE.TaskResult'error -> Right M.TaskError
  CGE.TaskResult'notStarted -> Right M.TaskNotStarted
  CGE.TaskResult'cancelled -> Right M.TaskCancelled
  CGE.TaskResult'unknown' n -> Left (WireUnknownEnum "TaskResult" n)

-- ---------------------------------------------------------------------
-- NodeAdminState
-- ---------------------------------------------------------------------

toCapnpNodeAdminState :: M.NodeAdminState -> CGE.NodeAdminState
toCapnpNodeAdminState = \case
  M.NodeOnline -> CGE.NodeAdminState'online
  M.NodeDraining -> CGE.NodeAdminState'draining
  M.NodeMaintenance -> CGE.NodeAdminState'maintenance

fromCapnpNodeAdminState :: CGE.NodeAdminState -> Either WireError M.NodeAdminState
fromCapnpNodeAdminState = \case
  CGE.NodeAdminState'online -> Right M.NodeOnline
  CGE.NodeAdminState'draining -> Right M.NodeDraining
  CGE.NodeAdminState'maintenance -> Right M.NodeMaintenance
  CGE.NodeAdminState'unknown' n -> Left (WireUnknownEnum "NodeAdminState" n)
