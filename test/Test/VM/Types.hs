{-# LANGUAGE OverloadedStrings #-}

-- | Types for VM-based integration tests.
module Test.VM.Types
  ( -- * VM Configuration
    VmConfig (..)
  , DefaultVmConfig (..)

    -- * Test VM handle
  , TestVm (..)
  )
where

import Corvus.Model (CacheType (..), DriveInterface (..), NetInterfaceType (..), SharedDirCache (..))
import Data.Int (Int64)
import Data.Text (Text)
import Test.Daemon (TestDaemon)

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
      , vmcOsName = "almalinux-10"
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
      , vmcHeadless = True
      }

-- | A VM running through the test daemon with SSH access
data TestVm = TestVm
  { tvmId :: !Int64
  -- ^ VM ID in the database
  , tvmDiskId :: !Int64
  -- ^ ID of the boot disk
  , tvmSshPort :: !Int
  -- ^ SSH port
  , tvmSshHost :: !String
  -- ^ SSH host (IP address of the VM)
  , tvmDaemon :: !TestDaemon
  -- ^ Test daemon reference
  , tvmSshPrivateKey :: !FilePath
  -- ^ Path to private key file for SSH access
  , tvmSshKeyId :: !Int64
  -- ^ SSH key ID in the daemon
  , tvmSshUser :: !Text
  -- ^ SSH user name
  }
