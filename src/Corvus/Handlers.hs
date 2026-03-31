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
  , module Corvus.Handlers.GuestExec
  , module Corvus.Handlers.Apply
  )
where

import Corvus.Handlers.Apply
import Corvus.Handlers.Core
import Corvus.Handlers.Disk
import Corvus.Handlers.GuestExec
import Corvus.Handlers.NetIf
import Corvus.Handlers.Network
import Corvus.Handlers.Resolve
import Corvus.Handlers.SharedDir
import Corvus.Handlers.SshKey
import Corvus.Handlers.Template
import Corvus.Handlers.Vm
import Corvus.Protocol
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)

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
  ReqShowVm vmRef -> withVm vmRef $ \vmId -> handleVmShow state vmId
  ReqVmCreate name cpus ram desc headless ga ci -> handleVmCreate state name cpus ram desc headless ga ci
  ReqVmDelete vmRef -> withVm vmRef $ \vmId -> handleVmDelete state vmId
  ReqVmStart vmRef -> withVm vmRef $ \vmId -> handleVmStart state vmId
  ReqVmStop vmRef -> withVm vmRef $ \vmId -> handleVmStop state vmId
  ReqVmPause vmRef -> withVm vmRef $ \vmId -> handleVmPause state vmId
  ReqVmReset vmRef -> withVm vmRef $ \vmId -> handleVmReset state vmId
  ReqVmEdit vmRef mCpus mRam mDesc mHeadless mGa mCi -> withVm vmRef $ \vmId -> handleVmEdit state vmId mCpus mRam mDesc mHeadless mGa mCi
  ReqVmCloudInit vmRef -> withVm vmRef $ \vmId -> handleVmCloudInit state vmId
  -- Disk image handlers
  ReqDiskCreate name format sizeMb mPath -> handleDiskCreate state name format sizeMb mPath
  ReqDiskCreateOverlay name baseDiskRef optPath -> withDisk baseDiskRef $ \baseDiskId -> handleDiskCreateOverlay state name baseDiskId optPath
  ReqDiskRegister name path mFormat -> handleDiskRegister state name path mFormat
  ReqDiskRefresh diskRef -> withDisk diskRef $ \diskId -> handleDiskRefresh state diskId
  ReqDiskDelete diskRef -> withDisk diskRef $ \diskId -> handleDiskDelete state diskId
  ReqDiskResize diskRef newSizeMb -> withDisk diskRef $ \diskId -> handleDiskResize state diskId newSizeMb
  ReqDiskList -> handleDiskList state
  ReqDiskShow diskRef -> withDisk diskRef $ \diskId -> handleDiskShow state diskId
  ReqDiskClone name baseDiskRef optionalPath -> withDisk baseDiskRef $ \baseDiskId -> handleDiskClone state name baseDiskId optionalPath
  -- Snapshot handlers
  ReqSnapshotCreate diskRef name -> withDisk diskRef $ \diskId -> handleSnapshotCreate state diskId name
  ReqSnapshotDelete diskRef snapRef -> withDisk diskRef $ \diskId -> handleSnapshotDelete state diskId snapRef
  ReqSnapshotRollback diskRef snapRef -> withDisk diskRef $ \diskId -> handleSnapshotRollback state diskId snapRef
  ReqSnapshotMerge diskRef snapRef -> withDisk diskRef $ \diskId -> handleSnapshotMerge state diskId snapRef
  ReqSnapshotList diskRef -> withDisk diskRef $ \diskId -> handleSnapshotList state diskId
  -- Attach/detach handlers
  ReqDiskAttach vmRef diskRef interface media readOnly discard cache ->
    withVmDisk vmRef diskRef $ \vmId diskId -> handleDiskAttach state vmId diskId interface media readOnly discard cache
  ReqDiskDetach vmRef diskRef ->
    withVmDisk vmRef diskRef $ \vmId diskId -> handleDiskDetachByDisk state vmId diskId
  -- Shared directory handlers
  ReqSharedDirAdd vmRef path tag cache readOnly -> withVm vmRef $ \vmId -> handleSharedDirAdd state vmId path tag cache readOnly
  ReqSharedDirRemove vmRef dirRef -> do
    r1 <- resolveVm vmRef pool
    case r1 of
      Left _ -> pure RespVmNotFound
      Right vmId -> do
        r2 <- resolveSharedDir dirRef vmId pool
        case r2 of
          Left _ -> pure RespSharedDirNotFound
          Right dirId -> handleSharedDirRemove state vmId dirId
  ReqSharedDirList vmRef -> withVm vmRef $ \vmId -> handleSharedDirList state vmId
  -- Network interface handlers
  ReqNetIfAdd vmRef ifaceType hostDev mac mNwRef -> do
    r1 <- resolveVm vmRef pool
    case r1 of
      Left _ -> pure RespVmNotFound
      Right vmId -> do
        mNwId <- resolveOptionalNetwork mNwRef
        case mNwId of
          Left _ -> pure RespNetworkNotFound
          Right nwId -> handleNetIfAdd state vmId ifaceType hostDev mac nwId
  ReqNetIfRemove vmRef netIfId -> withVm vmRef $ \vmId -> handleNetIfRemove state vmId netIfId
  ReqNetIfList vmRef -> withVm vmRef $ \vmId -> handleNetIfList state vmId
  -- SSH key handlers
  ReqSshKeyCreate name publicKey -> handleSshKeyCreate state name publicKey
  ReqSshKeyDelete keyRef -> withSshKey keyRef $ \keyId -> handleSshKeyDelete state keyId
  ReqSshKeyList -> handleSshKeyList state
  ReqSshKeyAttach vmRef keyRef -> withVmSshKey vmRef keyRef $ \vmId keyId -> handleSshKeyAttach state vmId keyId
  ReqSshKeyDetach vmRef keyRef -> withVmSshKey vmRef keyRef $ \vmId keyId -> handleSshKeyDetach state vmId keyId
  ReqSshKeyListForVm vmRef -> withVm vmRef $ \vmId -> handleSshKeyListForVm state vmId
  -- Template handlers
  ReqTemplateCreate yaml -> handleTemplateCreate state yaml
  ReqTemplateDelete tRef -> withTemplate tRef $ \tid -> handleTemplateDelete state tid
  ReqTemplateList -> handleTemplateList state
  ReqTemplateShow tRef -> withTemplate tRef $ \tid -> handleTemplateShow state tid
  ReqTemplateInstantiate tRef name -> withTemplate tRef $ \tid -> handleTemplateInstantiate state tid name
  -- Network handlers
  ReqNetworkCreate name subnet -> handleNetworkCreate state name subnet
  ReqNetworkDelete nwRef -> withNetwork nwRef $ \nwId -> handleNetworkDelete state nwId
  ReqNetworkStart nwRef -> withNetwork nwRef $ \nwId -> handleNetworkStart state nwId
  ReqNetworkStop nwRef force -> withNetwork nwRef $ \nwId -> handleNetworkStop state nwId force
  ReqNetworkList -> handleNetworkList state
  ReqNetworkShow nwRef -> withNetwork nwRef $ \nwId -> handleNetworkShow state nwId
  -- Guest execution handlers
  ReqGuestExec vmRef cmd -> withVm vmRef $ \vmId -> handleGuestExec state vmId cmd
  -- Disk URL import
  ReqDiskImportUrl name url mFmt -> handleDiskImportUrl state name url mFmt
  -- Apply config
  ReqApply yaml skipExisting -> handleApply state yaml skipExisting
  where
    pool = ssDbPool state

    -- Single-entity resolution helpers (return domain-specific "not found")
    withVm :: Ref -> (Int64 -> IO Response) -> IO Response
    withVm ref f = resolveVm ref pool >>= either (const $ pure RespVmNotFound) f

    withDisk :: Ref -> (Int64 -> IO Response) -> IO Response
    withDisk ref f = resolveDisk ref pool >>= either (const $ pure RespDiskNotFound) f

    withNetwork :: Ref -> (Int64 -> IO Response) -> IO Response
    withNetwork ref f = resolveNetwork ref pool >>= either (const $ pure RespNetworkNotFound) f

    withSshKey :: Ref -> (Int64 -> IO Response) -> IO Response
    withSshKey ref f = resolveSshKey ref pool >>= either (const $ pure RespSshKeyNotFound) f

    withTemplate :: Ref -> (Int64 -> IO Response) -> IO Response
    withTemplate ref f = resolveTemplate ref pool >>= either (const $ pure RespTemplateNotFound) f

    -- Two-entity resolution helpers
    withVmDisk :: Ref -> Ref -> (Int64 -> Int64 -> IO Response) -> IO Response
    withVmDisk vmRef diskRef f = do
      r1 <- resolveVm vmRef pool
      r2 <- resolveDisk diskRef pool
      case (r1, r2) of
        (Right vmId, Right diskId) -> f vmId diskId
        (Left _, _) -> pure RespVmNotFound
        (_, Left _) -> pure RespDiskNotFound

    withVmSshKey :: Ref -> Ref -> (Int64 -> Int64 -> IO Response) -> IO Response
    withVmSshKey vmRef keyRef f = do
      r1 <- resolveVm vmRef pool
      r2 <- resolveSshKey keyRef pool
      case (r1, r2) of
        (Right vmId, Right keyId) -> f vmId keyId
        (Left _, _) -> pure RespVmNotFound
        (_, Left _) -> pure RespSshKeyNotFound

    withDiskSnapshot :: Ref -> Ref -> (Int64 -> Int64 -> IO Response) -> IO Response
    withDiskSnapshot diskRef snapRef f = do
      r1 <- resolveDisk diskRef pool
      case r1 of
        Left _ -> pure RespDiskNotFound
        Right diskId -> do
          r2 <- resolveSnapshot snapRef diskId pool
          case r2 of
            Left _ -> pure RespSnapshotNotFound
            Right snapId -> f diskId snapId

    resolveOptionalNetwork :: Maybe Ref -> IO (Either Text (Maybe Int64))
    resolveOptionalNetwork Nothing = pure $ Right Nothing
    resolveOptionalNetwork (Just nwRef) = do
      r <- resolveNetwork nwRef pool
      case r of
        Left err -> pure $ Left err
        Right nwId -> pure $ Right (Just nwId)
