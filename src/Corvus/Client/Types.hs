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
  | VmShow !Text
  | -- | Create a new VM (name, cpuCount, ramMb, description, headless, guestAgent, cloudInit)
    VmCreate !Text !Int !Int !(Maybe Text) !Bool !Bool !Bool
  | -- | Delete a VM
    VmDelete !Text
  | VmStart !Text !WaitOptions
  | VmStop !Text !WaitOptions
  | VmPause !Text
  | VmReset !Text
  | -- | View VM via SPICE (runs remote-viewer)
    VmView !Text
  | -- | Connect to VM's HMP monitor
    VmMonitor !Text
  | -- | Edit VM properties (vmRef, cpuCount, ramMb, description, headless, guestAgent, cloudInit)
    VmEdit !Text !(Maybe Int) !(Maybe Int) !(Maybe Text) !(Maybe Bool) !(Maybe Bool) !(Maybe Bool)
  | -- | Generate/regenerate cloud-init ISO for a VM
    VmCloudInit !Text
  | -- | Execute a command in a VM via guest agent (vmRef, command)
    VmExec !Text !Text
  | -- Disk image commands

    -- | Create disk image (name, format, sizeMb, optionalPath)
    DiskCreate !Text !Text !Int64 !(Maybe Text)
  | -- | Import existing disk image (name, path, optional format)
    DiskImport !Text !FilePath !(Maybe Text)
  | -- | Create overlay disk image (name, baseDiskRef, optionalDirPath)
    DiskCreateOverlay !Text !Text !(Maybe Text)
  | -- | Refresh disk image size from qemu-img info
    DiskRefresh !Text
  | -- | Delete disk image
    DiskDelete !Text
  | -- | Resize disk image (diskRef, newSizeMb)
    DiskResize !Text !Int64
  | -- | List all disk images
    DiskList
  | -- | Show disk image details
    DiskShow !Text
  | -- | Clone disk image (name, baseDiskRef, optionalPath)
    DiskClone !Text !Text !(Maybe Text)
  | -- | Attach disk to VM (vmRef, diskRef, interface, media, readOnly, discard, cache)
    DiskAttach !Text !Text !Text !(Maybe Text) !Bool !Bool !Text
  | -- | Detach disk from VM (vmRef, diskRef)
    DiskDetach !Text !Text
  | -- Shared directory commands

    -- | Add shared directory to VM (vmRef, path, tag, cache, readOnly)
    SharedDirAdd !Text !Text !Text !Text !Bool
  | -- | Remove shared directory from VM (vmRef, sharedDirRef)
    SharedDirRemove !Text !Text
  | -- | List shared directories for VM
    SharedDirList !Text
  | -- Network interface commands

    -- | Add network interface to VM (vmRef, type, hostDevice, mac, networkRef)
    NetIfAdd !Text !Text !Text !(Maybe Text) !(Maybe Text)
  | -- | Remove network interface from VM (vmRef, netIfId)
    NetIfRemove !Text !Int64
  | -- | List network interfaces for VM
    NetIfList !Text
  | -- Snapshot commands

    -- | Create snapshot (diskRef, name)
    SnapshotCreate !Text !Text
  | -- | Delete snapshot (diskRef, snapshotRef)
    SnapshotDelete !Text !Text
  | -- | Rollback to snapshot (diskRef, snapshotRef)
    SnapshotRollback !Text !Text
  | -- | Merge snapshot (diskRef, snapshotRef)
    SnapshotMerge !Text !Text
  | -- | List snapshots (diskRef)
    SnapshotList !Text
  | -- SSH key commands

    -- | Create SSH key (name, publicKey)
    SshKeyCreate !Text !Text
  | -- | Delete SSH key (keyRef)
    SshKeyDelete !Text
  | -- | List all SSH keys
    SshKeyList
  | -- | Attach SSH key to VM (vmRef, keyRef)
    SshKeyAttach !Text !Text
  | -- | Detach SSH key from VM (vmRef, keyRef)
    SshKeyDetach !Text !Text
  | -- | List SSH keys for VM (vmRef)
    SshKeyListForVm !Text
  | -- Template commands

    -- | Create template from YAML file (file path)
    TemplateCreate !FilePath
  | -- | Delete template (templateRef)
    TemplateDelete !Text
  | -- | List all templates
    TemplateList
  | -- | Show template details (templateRef)
    TemplateShow !Text
  | -- | Instantiate a template (templateRef, new vm name)
    TemplateInstantiate !Text !Text
  | -- Virtual network commands

    -- | Create a virtual network (name, subnet, dhcp)
    NetworkCreate !Text !Text !Bool
  | -- | Delete a virtual network (networkRef)
    NetworkDelete !Text
  | -- | Start a virtual network (networkRef)
    NetworkStart !Text
  | -- | Stop a virtual network (networkRef, force)
    NetworkStop !Text !Bool
  | -- | List all virtual networks
    NetworkList
  | -- | Show virtual network details (networkRef)
    NetworkShow !Text
  | -- Apply commands

    -- | Apply environment from YAML config file (file path, skipExisting)
    Apply !FilePath !Bool
  | -- Task history commands

    -- | List task history (limit, optional subsystem, optional result)
    TaskList !Int !(Maybe Text) !(Maybe Text)
  | -- | Show single task details (taskId)
    TaskShow !Int64
  | -- | Wait for task to complete (taskId, optional timeout in seconds)
    TaskWait !Int64 !(Maybe Int)
  | -- | Generate shell completion script (bash, zsh, fish)
    Completion !Text
  deriving (Show)
