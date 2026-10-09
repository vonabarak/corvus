{-# LANGUAGE OverloadedStrings #-}

-- | DSL primitives for test setup (Given phase).
-- Provides functions to insert test data into the database.
module Test.DSL.Given
  ( -- * VM setup
    insertVm
  , insertRunningVmWithGuestAgent
  , setVmTpm
  , givenVmExists
  , givenCloudInitVmExists

    -- * Disk image setup
  , insertDiskImage
  , insertDiskImageWithBacking
  , insertDiskImageOnTestNode

    -- * Drive setup
  , attachDrive
  , attachDriveFull
  , attachCdromDrive

    -- * Snapshot setup
  , insertSnapshot
  , insertSnapshotWithVmstate

    -- * Network setup
  , insertNetwork

    -- * Network interface setup
  , insertNetworkInterface

    -- * Shared directory setup
  , insertSharedDir

    -- * SSH key setup
  , insertSshKey
  , attachSshKeyToVm

    -- * Node setup
  , seedTestNode
  , setTestNodeNetdDisabled
  )
where

import Control.Monad.IO.Class (liftIO)
import Corvus.Images
import Corvus.Model
import qualified Corvus.Model as M
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist (getBy, insert)
import qualified Database.Persist
import Test.DSL.Core (TestM, runDb)

--------------------------------------------------------------------------------
-- Test Node
--------------------------------------------------------------------------------

-- | Ensure a single test 'Node' row exists and return its key.
-- Every Vm and Network the DSL inserts references this node, so
-- the per-test DB has a coherent FK graph. Idempotent: subsequent
-- calls within the same test return the existing key.
seedTestNode :: TestM NodeId
seedTestNode = do
  mExisting <- runDb $ getBy (M.UniqueNodeName "test-node")
  case mExisting of
    Just (Entity k _) -> pure k
    Nothing -> do
      now <- liftIO getCurrentTime
      runDb $
        insert
          Node
            { nodeName = "test-node"
            , nodeHost = "127.0.0.1"
            , nodeNodeAgentPort = 9878
            , nodeNetAgentPort = 9877
            , nodeBasePath = "/tmp"
            , nodeDescription = Nothing
            , nodeAdminState = NodeOnline
            , nodeCreatedAt = now
            , nodeCpuCount = Nothing
            , nodeRamTotal = Nothing
            , nodeRamFree = Nothing
            , nodeStorageBytesTotal = Nothing
            , nodeStorageBytesFree = Nothing
            , nodeLoadAvg1 = Nothing
            , nodeLoadAvg5 = Nothing
            , nodeLoadAvg15 = Nothing
            , nodeKernelRelease = Nothing
            , nodeAgentVersion = Nothing
            , nodeNodeAgentHealthcheck = Nothing
            , nodeNetAgentHealthcheck = Nothing
            , nodeNetdDisabled = False
            }

-- | Flip the seeded test node's 'nodeNetdDisabled' flag. Tests
-- that exercise the netd-disabled gates call this after the
-- 'seedTestNode' (or any helper that triggers it via FK) ran.
setTestNodeNetdDisabled :: Bool -> TestM ()
setTestNodeNetdDisabled disabled = do
  nid <- seedTestNode
  runDb $
    Database.Persist.update
      nid
      [M.NodeNetdDisabled Database.Persist.=. disabled]

--------------------------------------------------------------------------------
-- VM Setup
--------------------------------------------------------------------------------

setVmTpm :: Int64 -> Bool -> TestM ()
setVmTpm vmId enabled =
  runDb $
    Database.Persist.update
      (toSqlKey vmId :: VmId)
      [M.VmTpm Database.Persist.=. enabled]

-- | Insert a VM with minimal parameters
insertVm :: Text -> VmStatus -> TestM Int64
insertVm name status = do
  nodeKey <- seedTestNode
  now <- liftIO getCurrentTime
  key <-
    runDb $
      insert
        Vm
          { vmName = name
          , vmNodeId = nodeKey
          , vmCreatedAt = now
          , vmStatus = status
          , vmLifecycleRevision = 0
          , vmRuntimeGeneration = Nothing
          , vmCpuCount = 2
          , vmRam = 4294967296
          , vmDescription = Nothing
          , vmHeadless = False
          , vmGuestAgent = False
          , vmTpm = False
          , vmCloudInit = False
          , vmHealthcheck = Nothing
          , vmAutostart = False
          , vmSpicePort = Nothing
          , vmVsockCid = Nothing
          , vmErrorMessage = Nothing
          , vmLastErrorAt = Nothing
          , vmRebootQuirk = False
          , vmCpuModel = "host"
          , vmGraphicsAdapter = GraphicsVirtioVga
          , vmVsock = True
          , vmBalloon = True
          , vmRng = True
          }
  pure $ fromSqlKey key

-- | Insert a running VM with the guest-agent bit flipped on.
-- Used by `whenGuestExec` tests; the bare `insertVm` helper
-- defaults `vmGuestAgent = False`.
insertRunningVmWithGuestAgent :: Text -> TestM Int64
insertRunningVmWithGuestAgent name = do
  nodeKey <- seedTestNode
  now <- liftIO getCurrentTime
  key <-
    runDb $
      insert
        Vm
          { vmName = name
          , vmNodeId = nodeKey
          , vmCreatedAt = now
          , vmStatus = VmRunning
          , vmLifecycleRevision = 0
          , vmRuntimeGeneration = Nothing
          , vmCpuCount = 2
          , vmRam = 4294967296
          , vmDescription = Nothing
          , vmHeadless = True
          , vmGuestAgent = True
          , vmTpm = False
          , vmCloudInit = False
          , vmHealthcheck = Nothing
          , vmAutostart = False
          , vmSpicePort = Nothing
          , vmVsockCid = Nothing
          , vmErrorMessage = Nothing
          , vmLastErrorAt = Nothing
          , vmRebootQuirk = False
          , vmCpuModel = "host"
          , vmGraphicsAdapter = GraphicsVirtioVga
          , vmVsock = True
          , vmBalloon = True
          , vmRng = True
          }
  pure $ fromSqlKey key

--------------------------------------------------------------------------------
-- Disk Image Setup
--------------------------------------------------------------------------------

-- | Insert a disk image with minimal parameters
insertDiskImage :: Text -> DriveFormat -> TestM Int64
insertDiskImage name format = do
  now <- liftIO getCurrentTime
  key <-
    runDb $
      publishImage
        DiskImage
          { diskImageName = name
          , diskImageFormat = format
          , diskImageSize = Nothing
          , diskImageCreatedAt = now
          , diskImageBackingImageId = Nothing
          , diskImageEphemeral = False
          }
  pure $ fromSqlKey key

-- | Insert a disk image with an optional backing image (for overlay disks)
insertDiskImageWithBacking
  :: Text
  -> DriveFormat
  -> Maybe Int64
  -> Maybe Int64
  -> TestM Int64
insertDiskImageWithBacking name format size mBackingId = do
  now <- liftIO getCurrentTime
  key <-
    runDb $
      publishImage
        DiskImage
          { diskImageName = name
          , diskImageFormat = format
          , diskImageSize = size
          , diskImageCreatedAt = now
          , diskImageBackingImageId = fmap toSqlKey mBackingId
          , diskImageEphemeral = False
          }
  pure $ fromSqlKey key

-- | Insert a 'DiskImage' and a matching 'DiskImageNode' row
-- pinning it to the seeded test-node. Needed by handlers that
-- check the same-node invariant on attach / vm-start (see
-- 'Corvus.Handlers.Disk.Attach.handleDiskAttach'); the bare
-- 'insertDiskImage' helper above only writes the image row.
insertDiskImageOnTestNode :: Text -> Text -> DriveFormat -> TestM Int64
insertDiskImageOnTestNode name path format = do
  nodeKey <- seedTestNode
  now <- liftIO getCurrentTime
  diskKey <-
    runDb $
      publishImage
        DiskImage
          { diskImageName = name
          , diskImageFormat = format
          , diskImageSize = Nothing
          , diskImageCreatedAt = now
          , diskImageBackingImageId = Nothing
          , diskImageEphemeral = False
          }
  _ <-
    runDb $
      insert
        DiskImageNode
          { diskImageNodeDiskImageId = diskKey
          , diskImageNodeNodeId = nodeKey
          , diskImageNodeFilePath = path
          }
  pure $ fromSqlKey diskKey

--------------------------------------------------------------------------------
-- Drive Setup
--------------------------------------------------------------------------------

-- | Attach a drive to a VM with minimal parameters
attachDrive :: Int64 -> Int64 -> DriveInterface -> TestM Int64
attachDrive vmId diskImageId interface = do
  key <-
    runDb $
      insert
        Drive
          { driveVmId = toSqlKey vmId
          , driveDiskImageId = Just (toSqlKey diskImageId)
          , driveInterface = interface
          , driveMedia = Nothing
          , driveReadOnly = False
          , driveCacheType = CacheWriteback
          , driveDiscard = False
          }
  pure $ fromSqlKey key

-- | Attach a drive with full control over all fields
attachDriveFull
  :: Int64
  -> Int64
  -> DriveInterface
  -> Maybe DriveMedia
  -> Bool
  -> CacheType
  -> Bool
  -> TestM Int64
attachDriveFull vmId diskImageId interface media readOnly cache discard = do
  key <-
    runDb $
      insert
        Drive
          { driveVmId = toSqlKey vmId
          , driveDiskImageId = Just (toSqlKey diskImageId)
          , driveInterface = interface
          , driveMedia = media
          , driveReadOnly = readOnly
          , driveCacheType = cache
          , driveDiscard = discard
          }
  pure $ fromSqlKey key

-- | Attach a CD-ROM drive (media = @cdrom@) to a VM. The tray is
-- empty ('Nothing') when @mDiskImageId@ is 'Nothing' — i.e. the
-- drive was already ejected; otherwise it references the given disk
-- image. Used by the media eject / change handler tests.
attachCdromDrive :: Int64 -> Maybe Int64 -> TestM Int64
attachCdromDrive vmId mDiskImageId = do
  key <-
    runDb $
      insert
        Drive
          { driveVmId = toSqlKey vmId
          , driveDiskImageId = fmap toSqlKey mDiskImageId
          , driveInterface = InterfaceIde
          , driveMedia = Just MediaCdrom
          , driveReadOnly = True
          , driveCacheType = CacheWriteback
          , driveDiscard = False
          }
  pure $ fromSqlKey key

--------------------------------------------------------------------------------
-- Snapshot Setup
--------------------------------------------------------------------------------

-- | Insert a snapshot for a disk image
insertSnapshot :: Int64 -> Text -> TestM Int64
insertSnapshot diskImageId name = do
  now <- liftIO getCurrentTime
  key <-
    runDb $
      insert
        Snapshot
          { snapshotDiskImageId = toSqlKey diskImageId
          , snapshotName = name
          , snapshotCreatedAt = now
          , snapshotSize = Nothing
          , snapshotLive = False
          , snapshotQuiesced = False
          , snapshotHasVmstate = False
          }
  pure $ fromSqlKey key

-- | Insert a snapshot row with the carrier-vmstate flag set, used
-- by VM-scoped snapshot tests that need a pre-existing
-- @hasVmstate=True@ row.
insertSnapshotWithVmstate :: Int64 -> Text -> Bool -> TestM Int64
insertSnapshotWithVmstate diskImageId name hasVmstate = do
  now <- liftIO getCurrentTime
  key <-
    runDb $
      insert
        Snapshot
          { snapshotDiskImageId = toSqlKey diskImageId
          , snapshotName = name
          , snapshotCreatedAt = now
          , snapshotSize = Nothing
          , snapshotLive = True
          , snapshotQuiesced = False
          , snapshotHasVmstate = hasVmstate
          }
  pure $ fromSqlKey key

--------------------------------------------------------------------------------
-- Network Setup
--------------------------------------------------------------------------------

-- | Insert a network into the database
insertNetwork :: Text -> Text -> TestM Int64
insertNetwork name subnet = do
  nodeKey <- seedTestNode
  now <- liftIO getCurrentTime
  key <-
    runDb $
      insert
        Network
          { networkName = name
          , networkNodeId = nodeKey
          , networkSubnet = subnet
          , networkDhcp = False
          , networkNat = False
          , networkRunning = False
          , networkDnsmasqPid = Nothing
          , networkCreatedAt = now
          , networkAutostart = False
          , networkVni = Nothing
          , networkDnsServers = ""
          , networkDomain = ""
          , networkHostDns = True
          }
  pure $ fromSqlKey key

--------------------------------------------------------------------------------
-- Network Interface Setup
--------------------------------------------------------------------------------

-- | Insert a network interface for a VM
insertNetworkInterface
  :: Int64
  -> NetInterfaceType
  -> Text
  -> Text
  -> TestM Int64
insertNetworkInterface vmId ifaceType hostDevice macAddress = do
  key <-
    runDb $
      insert
        NetworkInterface
          { networkInterfaceVmId = toSqlKey vmId
          , networkInterfaceInterfaceType = ifaceType
          , networkInterfaceModel = NetworkVirtioNetPci
          , networkInterfaceHostDevice = hostDevice
          , networkInterfaceMacAddress = macAddress
          , networkInterfaceNetworkId = Nothing
          , networkInterfaceGuestIpAddresses = Nothing
          , networkInterfaceIpAddress = Nothing
          }
  pure $ fromSqlKey key

--------------------------------------------------------------------------------
-- Shared Directory Setup
--------------------------------------------------------------------------------

-- | Insert a shared directory for a VM
insertSharedDir
  :: Int64
  -> Text
  -> Text
  -> SharedDirCache
  -> Bool
  -> TestM Int64
insertSharedDir vmId path tag cache readOnly = do
  key <-
    runDb $
      insert
        SharedDir
          { sharedDirVmId = toSqlKey vmId
          , sharedDirPath = path
          , sharedDirTag = tag
          , sharedDirCache = cache
          , sharedDirReadOnly = readOnly
          }
  pure $ fromSqlKey key

--------------------------------------------------------------------------------
-- Convenience Wrappers (given* functions)
--------------------------------------------------------------------------------

-- | Create a stopped VM with the given name
givenVmExists :: Text -> TestM Int64
givenVmExists name = insertVm name VmStopped

-- | Create a stopped VM with cloud-init enabled
givenCloudInitVmExists :: Text -> TestM Int64
givenCloudInitVmExists name = do
  nodeKey <- seedTestNode
  now <- liftIO getCurrentTime
  key <-
    runDb $
      insert
        Vm
          { vmName = name
          , vmNodeId = nodeKey
          , vmCreatedAt = now
          , vmStatus = VmStopped
          , vmLifecycleRevision = 0
          , vmRuntimeGeneration = Nothing
          , vmCpuCount = 2
          , vmRam = 4294967296
          , vmDescription = Nothing
          , vmHeadless = False
          , vmGuestAgent = False
          , vmTpm = False
          , vmCloudInit = True
          , vmHealthcheck = Nothing
          , vmAutostart = False
          , vmSpicePort = Nothing
          , vmVsockCid = Nothing
          , vmErrorMessage = Nothing
          , vmLastErrorAt = Nothing
          , vmRebootQuirk = False
          , vmCpuModel = "host"
          , vmGraphicsAdapter = GraphicsVirtioVga
          , vmVsock = True
          , vmBalloon = True
          , vmRng = True
          }
  pure $ fromSqlKey key

--------------------------------------------------------------------------------
-- SSH Key Setup
--------------------------------------------------------------------------------

-- | Insert an SSH key with name and public key
insertSshKey :: Text -> Text -> TestM Int64
insertSshKey name publicKey = do
  now <- liftIO getCurrentTime
  key <-
    runDb $
      insert
        SshKey
          { sshKeyName = name
          , sshKeyPublicKey = publicKey
          , sshKeyCreatedAt = now
          }
  pure $ fromSqlKey key

-- | Attach an SSH key to a VM
attachSshKeyToVm :: Int64 -> Int64 -> TestM Int64
attachSshKeyToVm vmId keyId = do
  key <-
    runDb $
      insert
        VmSshKey
          { vmSshKeyVmId = toSqlKey vmId
          , vmSshKeySshKeyId = toSqlKey keyId
          }
  pure $ fromSqlKey key
