{-# LANGUAGE OverloadedStrings #-}

-- | Types for VM-based integration tests.
module Test.VM.Types
  ( -- * VM Configuration
    VmConfig (..)
  , DefaultVmConfig (..)
  , biosVmConfig
  , cloudVmConfig
  , prebakedImageName
  , prebakedSshKeyPath

    -- * Test VM handle
  , TestVm (..)
  , SshLocator (..)
  )
where

import Corvus.Model (CacheType (..), DriveInterface (..), NetInterfaceType (..), SharedDirCache (..))
import Data.Int (Int64)
import Data.Text (Text)
import Test.VM.Daemon (TestDaemon)

-- | Pre-baked image name (local, no download)
prebakedImageName :: Text
prebakedImageName = "corvus-test"

-- | Path to pre-baked SSH private key (relative to project root)
prebakedSshKeyPath :: FilePath
prebakedSshKeyPath = ".test-images/corvus-test-key"

-- | Configuration for a test VM.
data VmConfig = VmConfig
  { vmcCpuCount :: Int
  , vmcRamMb :: Int
  , vmcOsName :: Text
  , vmcSharedDir :: Maybe FilePath
  , vmcDescription :: Maybe Text
  , vmcDiskInterface :: DriveInterface
  , vmcNetworkType :: NetInterfaceType
  , vmcWaitSshTimeout :: Int
  , vmcDiskCache :: CacheType
  , vmcDiskDiscard :: Bool
  , vmcSharedDirCache :: SharedDirCache
  , vmcSshUser :: Text
  , vmcAdditionalDisks :: [(Int64, DriveInterface, Bool)]
  , vmcHeadless :: Bool
  , vmcNetworkId :: Maybe Int64
  , vmcPrebakedSshKey :: Maybe FilePath
  -- ^ Pre-baked SSH private key path. When set, skip cloud-init key setup.
  , vmcUefi :: Bool
  -- ^ Use UEFI boot (adds OVMF firmware disks). Default: True.
  , vmcForceTcpSsh :: Bool
  -- ^ Force SSH over TCP (user-mode netdev + hostfwd) instead of
  -- vsock. Default: False. Tests that exercise networking
  -- (Apply/Network/SerialConsole) set this to True.
  , vmcWantUserNetdev :: Bool
  -- ^ Add a user-mode netdev for the guest's outbound networking
  -- even when SSH goes over vsock. Default: False. Cloud-init tests
  -- enable this so the metadata server can reach the guest, while
  -- not adding a hostfwd.
  }
  deriving (Show, Eq)

-- | Type class for default VM configuration.
class DefaultVmConfig a where
  defaultVmConfig :: a

instance DefaultVmConfig VmConfig where
  defaultVmConfig =
    VmConfig
      { vmcCpuCount = 2
      , vmcRamMb = 2048
      , vmcOsName = prebakedImageName
      , vmcSharedDir = Nothing
      , vmcDescription = Nothing
      , vmcDiskInterface = InterfaceVirtio
      , vmcNetworkType = NetUser
      , vmcWaitSshTimeout = 120
      , vmcDiskCache = CacheWriteback
      , vmcDiskDiscard = True
      , vmcSharedDirCache = CacheAuto
      , vmcSshUser = "corvus"
      , vmcAdditionalDisks = []
      , vmcHeadless = False
      , vmcNetworkId = Nothing
      , vmcPrebakedSshKey = Just prebakedSshKeyPath
      , vmcUefi = True
      , vmcForceTcpSsh = False
      , vmcWantUserNetdev = False
      }

-- | VM config for cloud-image tests (uses cloud-init for SSH key deployment).
cloudVmConfig :: VmConfig
cloudVmConfig =
  defaultVmConfig
    { vmcOsName = "almalinux-10"
    , vmcWaitSshTimeout = 120
    , vmcPrebakedSshKey = Nothing
    }

-- | VM config for BIOS boot (no UEFI firmware).
biosVmConfig :: VmConfig
biosVmConfig = defaultVmConfig {vmcUefi = False}

-- | How a test SSHes into the VM. Tests that don't need TCP-level
-- networking use 'SshVsock'; tests that exercise NAT/bridge
-- networking use 'SshTcp'; guest-exec-only tests use 'SshDisabled'.
data SshLocator
  = -- | TCP host and port (user-mode netdev + hostfwd).
    SshTcp !String !Int
  | -- | AF_VSOCK CID assigned to the VM.
    SshVsock !Int
  | -- | No SSH path (guest-exec-only test paths).
    SshDisabled
  deriving (Show, Eq)

-- | A VM running through the test daemon with SSH access
data TestVm = TestVm
  { tvmId :: !Int64
  -- ^ VM ID in the database
  , tvmDiskId :: !Int64
  -- ^ ID of the boot disk
  , tvmSsh :: !SshLocator
  -- ^ How to reach SSH on this VM
  , tvmDaemon :: !TestDaemon
  -- ^ Test daemon reference
  , tvmSshPrivateKey :: !FilePath
  -- ^ Path to private key file for SSH access
  , tvmSshKeyId :: !Int64
  -- ^ SSH key ID in the daemon
  , tvmSshUser :: !Text
  -- ^ SSH user name
  }
