-- | Types for the Corvus client CLI.
module Corvus.Client.Types
  ( -- * Command line options
    Options (..)
  , Command (..)

    -- * Output format
  , OutputFormat (..)

    -- * Wait options
  , WaitOptions (..)
  , defaultWaitOptions
  )
where

import Data.Int (Int64)
import Data.Text (Text)

-- | Output format for CLI commands
data OutputFormat = TextOutput | JsonOutput | YamlOutput
  deriving (Show, Eq)

-- | Options for blocking until an async operation completes.
data WaitOptions = WaitOptions
  { woWait :: !Bool
  , woTimeout :: !(Maybe Int)
  -- ^ Timeout in seconds. Nothing = default 120s.
  }
  deriving (Show, Eq)

-- | Default wait options (no waiting)
defaultWaitOptions :: WaitOptions
defaultWaitOptions = WaitOptions False Nothing

-- | Command line options
data Options = Options
  { optSocket :: Maybe FilePath
  , optTcp :: Bool
  , optHost :: String
  , optPort :: Int
  , optOutput :: OutputFormat
  , optCommand :: Command
  }
  deriving (Show)

-- | Available commands
data Command
  = Ping
  | Status
  | Shutdown
  | -- VM commands
    VmList
  | VmShow !Int64
  | -- | Create a new VM (name, cpuCount, ramMb, description, headless, guestAgent, cloudInit)
    VmCreate !Text !Int !Int !(Maybe Text) !Bool !Bool !Bool
  | -- | Delete a VM
    VmDelete !Int64
  | VmStart !Int64
  | VmStop !Int64 !WaitOptions
  | VmPause !Int64
  | VmReset !Int64
  | -- | View VM via SPICE (runs remote-viewer)
    VmView !Int64
  | -- | Connect to VM's HMP monitor
    VmMonitor !Int64
  | -- | Edit VM properties (vmId, cpuCount, ramMb, description, headless, guestAgent, cloudInit)
    VmEdit !Int64 !(Maybe Int) !(Maybe Int) !(Maybe Text) !(Maybe Bool) !(Maybe Bool) !(Maybe Bool)
  | -- | Generate/regenerate cloud-init ISO for a VM (vmId)
    VmCloudInit !Int64
  | -- | Execute a command in a VM via guest agent (vmId, command)
    VmExec !Int64 !Text
  | -- Disk image commands

    -- | Create disk image (name, format, sizeMb)
    DiskCreate !Text !Text !Int64
  | -- | Import existing disk image (name, path, format)
    DiskImport !Text !FilePath !(Maybe Text)
  | -- | Create overlay disk image (name, baseDiskId, optionalDirPath)
    DiskCreateOverlay !Text !Int64 !(Maybe Text)
  | -- | Delete disk image
    DiskDelete !Int64
  | -- | Resize disk image (diskId, newSizeMb)
    DiskResize !Int64 !Int64
  | -- | List all disk images
    DiskList
  | -- | Show disk image details
    DiskShow !Int64
  | -- | Clone disk image (name, baseDiskId, optionalPath)
    DiskClone !Text !Int64 !(Maybe Text)
  | -- | Attach disk to VM (vmId, diskId, interface, media, readOnly, discard, cache)
    DiskAttach !Int64 !Int64 !Text !(Maybe Text) !Bool !Bool !Text
  | -- | Detach disk from VM (vmId, driveId)
    DiskDetach !Int64 !Int64
  | -- Shared directory commands

    -- | Add shared directory to VM (vmId, path, tag, cache, readOnly)
    SharedDirAdd !Int64 !Text !Text !Text !Bool
  | -- | Remove shared directory from VM (vmId, sharedDirId)
    SharedDirRemove !Int64 !Int64
  | -- | List shared directories for VM
    SharedDirList !Int64
  | -- Network interface commands

    -- | Add network interface to VM (vmId, type, hostDevice, mac, networkId)
    NetIfAdd !Int64 !Text !Text !(Maybe Text) !(Maybe Int64)
  | -- | Remove network interface from VM (vmId, netIfId)
    NetIfRemove !Int64 !Int64
  | -- | List network interfaces for VM
    NetIfList !Int64
  | -- Snapshot commands

    -- | Create snapshot (diskId, name)
    SnapshotCreate !Int64 !Text
  | -- | Delete snapshot (diskId, snapshotId)
    SnapshotDelete !Int64 !Int64
  | -- | Rollback to snapshot (diskId, snapshotId)
    SnapshotRollback !Int64 !Int64
  | -- | Merge snapshot (diskId, snapshotId)
    SnapshotMerge !Int64 !Int64
  | -- | List snapshots (diskId)
    SnapshotList !Int64
  | -- SSH key commands

    -- | Create SSH key (name, publicKey)
    SshKeyCreate !Text !Text
  | -- | Delete SSH key (keyId)
    SshKeyDelete !Int64
  | -- | List all SSH keys
    SshKeyList
  | -- | Attach SSH key to VM (vmId, keyId)
    SshKeyAttach !Int64 !Int64
  | -- | Detach SSH key from VM (vmId, keyId)
    SshKeyDetach !Int64 !Int64
  | -- | List SSH keys for VM (vmId)
    SshKeyListForVm !Int64
  | -- Template commands

    -- | Create template from YAML file (file path)
    TemplateCreate !FilePath
  | -- | Delete template (template id)
    TemplateDelete !Int64
  | -- | List all templates
    TemplateList
  | -- | Show template details (template id)
    TemplateShow !Int64
  | -- | Instantiate a template (template id, new vm name)
    TemplateInstantiate !Int64 !Text
  | -- Virtual network commands

    -- | Create a virtual network (name, subnet)
    NetworkCreate !Text !Text
  | -- | Delete a virtual network (networkId)
    NetworkDelete !Int64
  | -- | Start a virtual network (networkId)
    NetworkStart !Int64
  | -- | Stop a virtual network (networkId, force)
    NetworkStop !Int64 !Bool
  | -- | List all virtual networks
    NetworkList
  | -- | Show virtual network details (networkId)
    NetworkShow !Int64
  | -- Apply commands

    -- | Apply environment from YAML config file (file path)
    -- | Apply environment from YAML config file (file path, skipExisting)
    Apply !FilePath !Bool
  deriving (Show)
