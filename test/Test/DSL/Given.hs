{-# LANGUAGE OverloadedStrings #-}

-- | DSL primitives for test setup (Given phase).
-- Provides functions to insert test data into the database.
module Test.DSL.Given
  ( -- * VM setup
    insertVm
  , insertVmFull
  , insertHeadlessVm
  , insertRunningVmWithGuestAgent
  , givenVmExists
  , givenCloudInitVmExists
  , givenRunningVmExists

    -- * Disk image setup
  , insertDiskImage
  , insertDiskImageFull
  , insertDiskImageWithBacking
  , insertDiskImageOnTestNode
  , givenDiskExists
  , givenDiskOnTestNodeExists

    -- * Drive setup
  , attachDrive
  , attachDriveFull

    -- * Snapshot setup
  , insertSnapshot
  , givenSnapshotExists

    -- * Network setup
  , insertNetwork

    -- * Network interface setup
  , insertNetworkInterface

    -- * Shared directory setup
  , insertSharedDir
  , givenSharedDirExists

    -- * SSH key setup
  , insertSshKey
  , givenSshKeyExists
  , attachSshKeyToVm

    -- * Node setup
  , seedTestNode
  , setTestNodeNetdDisabled

    -- * Utilities
  , defaultVm
  , defaultDiskImage
  , defaultDrive
  )
where

import Control.Monad.IO.Class (liftIO)
import Corvus.Model
import qualified Corvus.Model as M
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist (Key, getBy, insert)
import qualified Database.Persist
import Database.Persist.Sql (Entity (..), fromSqlKey, toSqlKey)
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
            , nodeRamMbTotal = Nothing
            , nodeRamMbFree = Nothing
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
          , vmCpuCount = 2
          , vmRamMb = 4096
          , vmDescription = Nothing
          , vmHeadless = False
          , vmGuestAgent = False
          , vmCloudInit = False
          , vmHealthcheck = Nothing
          , vmAutostart = False
          , vmSpicePort = Nothing
          , vmVsockCid = Nothing
          , vmErrorMessage = Nothing
          , vmLastErrorAt = Nothing
          , vmMigrating = False
          , vmRebootQuirk = False
          , vmCpuModel = "host"
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
          , vmCpuCount = 2
          , vmRamMb = 4096
          , vmDescription = Nothing
          , vmHeadless = True
          , vmGuestAgent = True
          , vmCloudInit = False
          , vmHealthcheck = Nothing
          , vmAutostart = False
          , vmSpicePort = Nothing
          , vmVsockCid = Nothing
          , vmErrorMessage = Nothing
          , vmLastErrorAt = Nothing
          , vmMigrating = False
          , vmRebootQuirk = False
          , vmCpuModel = "host"
          }
  pure $ fromSqlKey key

-- | Insert a headless VM with the given name and status. Used by
-- SPICE-related tests that need to assert "no graphical console".
insertHeadlessVm :: Text -> VmStatus -> TestM Int64
insertHeadlessVm name status = do
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
          , vmCpuCount = 2
          , vmRamMb = 4096
          , vmDescription = Nothing
          , vmHeadless = True
          , vmGuestAgent = False
          , vmCloudInit = False
          , vmHealthcheck = Nothing
          , vmAutostart = False
          , vmSpicePort = Nothing
          , vmVsockCid = Nothing
          , vmErrorMessage = Nothing
          , vmLastErrorAt = Nothing
          , vmMigrating = False
          , vmRebootQuirk = False
          , vmCpuModel = "host"
          }
  pure $ fromSqlKey key

-- | Insert a VM with full control over all fields
insertVmFull
  :: Text
  -> VmStatus
  -> Int
  -> Int
  -> Maybe Text
  -> Maybe Int
  -> TestM Int64
insertVmFull name status cpus ramMb desc _pid = do
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
          , vmCpuCount = cpus
          , vmRamMb = ramMb
          , vmDescription = desc
          , vmHeadless = False
          , vmGuestAgent = False
          , vmCloudInit = False
          , vmHealthcheck = Nothing
          , vmAutostart = False
          , vmSpicePort = Nothing
          , vmVsockCid = Nothing
          , vmErrorMessage = Nothing
          , vmLastErrorAt = Nothing
          , vmMigrating = False
          , vmRebootQuirk = False
          , vmCpuModel = "host"
          }
  pure $ fromSqlKey key

-- | Default VM values for reference
-- | Stub VM used in pure tests that don't touch the DB.
-- Carries an unpersisted node id — callers comparing whole 'Vm'
-- values must mind this.
defaultVm :: IO Vm
defaultVm = do
  now <- getCurrentTime
  pure
    Vm
      { vmName = "test-vm"
      , vmNodeId = toSqlKey 1 :: NodeId
      , vmCreatedAt = now
      , vmStatus = VmStopped
      , vmCpuCount = 2
      , vmRamMb = 4096
      , vmDescription = Nothing
      , vmHeadless = False
      , vmGuestAgent = False
      , vmCloudInit = False
      , vmHealthcheck = Nothing
      , vmAutostart = False
      , vmSpicePort = Nothing
      , vmVsockCid = Nothing
      , vmErrorMessage = Nothing
      , vmLastErrorAt = Nothing
      , vmMigrating = False
      , vmRebootQuirk = False
      , vmCpuModel = "host"
      }

--------------------------------------------------------------------------------
-- Disk Image Setup
--------------------------------------------------------------------------------

-- | Insert a disk image with minimal parameters
insertDiskImage :: Text -> Text -> DriveFormat -> TestM Int64
insertDiskImage name path format = do
  now <- liftIO getCurrentTime
  key <-
    runDb $
      insert
        DiskImage
          { diskImageName = name
          , diskImageFormat = format
          , diskImageSizeMb = Nothing
          , diskImageCreatedAt = now
          , diskImageBackingImageId = Nothing
          , diskImageEphemeral = False
          }
  pure $ fromSqlKey key

-- | Insert a disk image with full control over all fields
insertDiskImageFull
  :: Text
  -> Text
  -> DriveFormat
  -> Maybe Int
  -> TestM Int64
insertDiskImageFull name path format sizeMb = do
  now <- liftIO getCurrentTime
  key <-
    runDb $
      insert
        DiskImage
          { diskImageName = name
          , diskImageFormat = format
          , diskImageSizeMb = sizeMb
          , diskImageCreatedAt = now
          , diskImageBackingImageId = Nothing
          , diskImageEphemeral = False
          }
  pure $ fromSqlKey key

-- | Insert a disk image with an optional backing image (for overlay disks)
insertDiskImageWithBacking
  :: Text
  -> Text
  -> DriveFormat
  -> Maybe Int
  -> Maybe Int64
  -> TestM Int64
insertDiskImageWithBacking name path format sizeMb mBackingId = do
  now <- liftIO getCurrentTime
  key <-
    runDb $
      insert
        DiskImage
          { diskImageName = name
          , diskImageFormat = format
          , diskImageSizeMb = sizeMb
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
      insert
        DiskImage
          { diskImageName = name
          , diskImageFormat = format
          , diskImageSizeMb = Nothing
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

-- | Default disk image values for reference
defaultDiskImage :: IO DiskImage
defaultDiskImage = do
  now <- getCurrentTime
  pure
    DiskImage
      { diskImageName = "test-disk"
      , diskImageFormat = FormatQcow2
      , diskImageSizeMb = Just 10240
      , diskImageCreatedAt = now
      , diskImageBackingImageId = Nothing
      , diskImageEphemeral = False
      }

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
          , driveDiskImageId = toSqlKey diskImageId
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
          , driveDiskImageId = toSqlKey diskImageId
          , driveInterface = interface
          , driveMedia = media
          , driveReadOnly = readOnly
          , driveCacheType = cache
          , driveDiscard = discard
          }
  pure $ fromSqlKey key

-- | Default drive values for reference
defaultDrive :: Int64 -> Int64 -> Drive
defaultDrive vmId diskImageId =
  Drive
    { driveVmId = toSqlKey vmId
    , driveDiskImageId = toSqlKey diskImageId
    , driveInterface = InterfaceVirtio
    , driveMedia = Just MediaDisk
    , driveReadOnly = False
    , driveCacheType = CacheWriteback
    , driveDiscard = False
    }

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
          , snapshotSizeMb = Nothing
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
          , vmCpuCount = 2
          , vmRamMb = 4096
          , vmDescription = Nothing
          , vmHeadless = False
          , vmGuestAgent = False
          , vmCloudInit = True
          , vmHealthcheck = Nothing
          , vmAutostart = False
          , vmSpicePort = Nothing
          , vmVsockCid = Nothing
          , vmErrorMessage = Nothing
          , vmLastErrorAt = Nothing
          , vmMigrating = False
          , vmRebootQuirk = False
          , vmCpuModel = "host"
          }
  pure $ fromSqlKey key

-- | Create a running VM with the given name
givenRunningVmExists :: Text -> TestM Int64
givenRunningVmExists name = insertVm name VmRunning

-- | Create a qcow2 disk image with the given name
givenDiskExists :: Text -> TestM Int64
givenDiskExists name = insertDiskImage name ("/test/images/" <> name <> ".qcow2") FormatQcow2

-- | Same as 'givenDiskExists' but also places the image on the
-- test-node via 'DiskImageNode' so 'handleDiskAttach' /
-- 'handleVmStart' can find it under the same-node invariant.
givenDiskOnTestNodeExists :: Text -> TestM Int64
givenDiskOnTestNodeExists name =
  insertDiskImageOnTestNode
    name
    ("/test/images/" <> name <> ".qcow2")
    FormatQcow2

-- | Create a snapshot for a disk
givenSnapshotExists :: Int64 -> Text -> TestM Int64
givenSnapshotExists = insertSnapshot

-- | Create a shared directory for a VM with default settings
givenSharedDirExists :: Int64 -> Text -> Text -> TestM Int64
givenSharedDirExists vmId path tag = insertSharedDir vmId path tag CacheAuto False

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

-- | Create an SSH key with default public key
givenSshKeyExists :: Text -> TestM Int64
givenSshKeyExists name = insertSshKey name ("ssh-ed25519 AAAA... " <> name)

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
