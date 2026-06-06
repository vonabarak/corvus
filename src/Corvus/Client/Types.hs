-- | Types for the Corvus client CLI.
module Corvus.Client.Types
  ( -- * Command line options
    Options (..)
  , Command (..)
  , BorderStyleOpt (..)

    -- * Output format
  , OutputFormat (..)

    -- * Wait options
  , WaitOptions (..)

    -- * Build client options
  , BuildClientOptions (..)
  , defaultBuildClientOptions

    -- * Snapshot options
  , QuiesceModeFlag (..)
  )
where

import Data.Int (Int64)
import Data.Text (Text)

-- | Output format for CLI commands
data OutputFormat = TextOutput | JsonOutput | YamlOutput
  deriving (Show, Eq)

-- | CLI-side mirror of the wire 'QuiesceMode' enum. Lives here to
-- keep 'Corvus.Client.Types' independent of the generated Cap'n
-- Proto modules — the Capnp.Gen.Enums type is the wire form;
-- 'Corvus.Client.Commands.Disk' translates this flag into it.
data QuiesceModeFlag
  = QuiesceFlagAuto
  | QuiesceFlagRequire
  | QuiesceFlagSkip
  deriving (Show, Eq)

-- | Options for blocking until an async operation completes.
data WaitOptions = WaitOptions
  { woWait :: !Bool
  , woTimeout :: !(Maybe Int)
  -- ^ Timeout in seconds. Nothing = default 120s.
  }
  deriving (Show, Eq)

-- | Cache-related knobs for @crv build@. These flags OR with the
-- per-build YAML fields (@useCache:@ / @buildCache:@); the
-- 'bcoRebuildFrom' field is CLI-only and caps the matched prefix
-- length when reusing a cache (0 = unset).
data BuildClientOptions = BuildClientOptions
  { bcoUseCache :: !Bool
  , bcoBuildCache :: !Bool
  , bcoRebuildFrom :: !Int
  }
  deriving (Show, Eq)

defaultBuildClientOptions :: BuildClientOptions
defaultBuildClientOptions = BuildClientOptions False False 0

-- | Command line options.
--
-- The client defaults to TCP at 127.0.0.1:9876. '--unix' (or a
-- '--socket PATH') switches to a Unix socket connection; the
-- 'CORVUS_HOST' / 'CORVUS_PORT' / 'CORVUS_SOCKET' env vars
-- override the corresponding defaults but are themselves
-- overridden by explicit CLI flags.
data Options = Options
  { optSocket :: Maybe FilePath
  , optUnix :: Bool
  , optHost :: String
  , optPort :: Int
  , optOutput :: OutputFormat
  , optBorders :: !BorderStyleOpt
  , optTruncate :: !Bool
  , optColumns :: ![String]
  , optFitWidth :: !Bool
  , optNoTls :: !Bool
  , optTlsCertDir :: !(Maybe FilePath)
  , optCommand :: Command
  }
  deriving (Show)

-- | Border style for table output. The client parser translates this into
-- 'Corvus.Client.Output.BorderStyle'; we keep them separate to avoid pulling
-- Output into Types (which is imported by the parser).
data BorderStyleOpt = BordersUnicodeOpt | BordersAsciiOpt | BordersNoneOpt
  deriving (Show, Eq)

-- | Available commands
data Command
  = Ping
  | Status
  | Shutdown
  | -- VM commands
    VmList
  | VmShow !Text
  | -- | Create a new VM (name, nodeRef, cpuCount, ramMb, description, headless, guestAgent, cloudInit, autostart, rebootQuirk, cpuModel)
    VmCreate !Text !Text !Int !Int !(Maybe Text) !Bool !Bool !Bool !Bool !Bool !Text
  | -- | Delete a VM (vmRef, keepDisks)
    VmDelete !Text !Bool
  | VmStart !Text !WaitOptions
  | VmStop !Text !WaitOptions
  | VmPause !Text
  | VmReset !Text
  | VmSave !Text !WaitOptions
  | -- | View VM via SPICE (runs remote-viewer)
    VmView !Text
  | -- | Connect to VM's HMP monitor
    VmMonitor !Text
  | -- | Edit VM properties (vmRef, cpuCount, ramMb, description, headless, guestAgent, cloudInit, autostart, rebootQuirk, cpuModel)
    VmEdit !Text !(Maybe Int) !(Maybe Int) !(Maybe Text) !(Maybe Bool) !(Maybe Bool) !(Maybe Bool) !(Maybe Bool) !(Maybe Bool) !(Maybe Text)
  | -- | Generate/regenerate cloud-init ISO for a VM
    CloudInitGenerate !Text
  | -- | Execute a command in a VM via guest agent (vmRef, command)
    VmExec !Text !Text
  | -- | Migrate a stopped VM to another node (vmRef, toNodeRef)
    VmMigrate !Text !Text
  | -- Disk image commands

    -- | Create disk image (name, format, sizeMb, optionalPath, ephemeral, nodeRef)
    DiskCreate !Text !Text !Int64 !(Maybe Text) !Bool !Text
  | -- | Register existing disk image in DB without copying (name, path, optional format, optional backing image ref, ephemeral, nodeRef)
    DiskRegisterCmd !Text !FilePath !(Maybe Text) !(Maybe Text) !Bool !Text
  | -- | Import disk image with copy/download (name, source, destPath, format, ephemeral, nodeRef, waitOptions)
    DiskImport !Text !Text !(Maybe Text) !(Maybe Text) !Bool !Text !WaitOptions
  | -- | Create overlay disk image (name, baseDiskRef, optionalDirPath, ephemeral)
    DiskCreateOverlay !Text !Text !(Maybe Text) !Bool
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
  | -- | Clone disk image (name, baseDiskRef, optionalPath, ephemeral)
    DiskClone !Text !Text !(Maybe Text) !Bool
  | -- | Rebase overlay to new backing or flatten (diskRef, newBackingRef, unsafe)
    DiskRebase !Text !(Maybe Text) !Bool
  | -- | Attach disk to VM (vmRef, diskRef, interface, media, readOnly, discard, cache)
    DiskAttach !Text !Text !Text !(Maybe Text) !Bool !Bool !Text
  | -- | Detach disk from VM (vmRef, diskRef)
    DiskDetach !Text !Text
  | -- | Copy a disk image to another node (diskRef, toNodeRef, optional toPath, withBackingChain)
    DiskCopy !Text !Text !(Maybe Text) !Bool
  | -- | Move a disk image to another node (diskRef, toNodeRef, optional toPath, withBackingChain)
    DiskMove !Text !Text !(Maybe Text) !Bool
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

    -- | Create snapshot (diskRef, name, quiesce mode).
    -- @quiesce@ only affects the live path (running/paused VM); on
    -- a stopped VM it is silently ignored because qemu-img writes
    -- the snapshot offline. See doc/snapshots.md.
    SnapshotCreate !Text !Text !QuiesceModeFlag
  | -- | Delete snapshot (diskRef, snapshotRef)
    SnapshotDelete !Text !Text
  | -- | Rollback to snapshot (diskRef, snapshotRef, autoStop).
    -- QEMU has no online rollback; @autoStop=True@ orchestrates a
    -- graceful VM stop + revert + start so the operator still
    -- issues a single command.
    SnapshotRollback !Text !Text !Bool
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

    -- | Create template from YAML file. 'Nothing' opens $EDITOR on a skeleton.
    TemplateCreate !(Maybe FilePath)
  | -- | Edit an existing template in $EDITOR (templateRef)
    TemplateEdit !Text
  | -- | Delete template (templateRef)
    TemplateDelete !Text
  | -- | List all templates
    TemplateList
  | -- | Show template details (templateRef)
    TemplateShow !Text
  | -- | Instantiate a template (templateRef, new vm name)
    TemplateInstantiate !Text !Text !Text
  | -- Virtual network commands

    -- | Create a virtual network (name, nodeRef, subnet, dhcp, nat, autostart)
    NetworkCreate !Text !Text !Text !Bool !Bool !Bool
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
  | -- | Edit network properties (networkRef, subnet, dhcp, nat, autostart)
    NetworkEdit !Text !(Maybe Text) !(Maybe Bool) !(Maybe Bool) !(Maybe Bool)
  | -- | Add a peer node to a network (networkRef, nodeRef)
    NetworkAttachNode !Text !Text
  | -- | Remove a peer node from a network (networkRef, nodeRef)
    NetworkDetachNode !Text !Text
  | -- Node commands

    -- | Add a node (name, host, nodeAgentPort, netAgentPort, basePath, description, adminState, netdDisabled).
    -- 'basePath' is 'Nothing' when the operator did not pass
    -- @--base-path@; the command handler resolves that to
    -- @$HOME/VMs@ on the admin host.
    NodeAdd !Text !Text !Int !Int !(Maybe Text) !(Maybe Text) !Text !Bool
  | -- | List all nodes
    NodeList
  | -- | Show node details (nodeRef)
    NodeShow !Text
  | -- | Edit node properties (nodeRef, name, host, nodeAgentPort, netAgentPort, basePath, description, adminState, netdDisabled).
    NodeEdit !Text !(Maybe Text) !(Maybe Text) !(Maybe Int) !(Maybe Int) !(Maybe Text) !(Maybe (Maybe Text)) !(Maybe Text) !(Maybe Bool)
  | -- | Mark a node as draining (nodeRef)
    NodeDrain !Text
  | -- | Delete a node (nodeRef)
    NodeDelete !Text
  | -- Cloud-init config commands

    -- | Set cloud-init config from YAML file. Nothing opens $EDITOR on a skeleton.
    CloudInitSet !Text !(Maybe FilePath)
  | -- | Edit cloud-init config in $EDITOR (vmRef)
    CloudInitEdit !Text
  | -- | Show cloud-init config (vmRef)
    CloudInitShow !Text
  | -- | Delete cloud-init config (vmRef)
    CloudInitDelete !Text
  | -- Apply commands

    -- | Apply environment from YAML config file (file path, skipExisting, waitOptions)
    Apply !FilePath !Bool !WaitOptions
  | -- | Build OS images from a YAML pipeline file. Async by default;
    -- @--wait@ blocks until completion. Cache flags layer on top of
    -- the YAML's own @useCache:@ / @buildCache:@ fields (OR semantics).
    Build !FilePath !BuildClientOptions !WaitOptions
  | -- Task history commands

    -- | List task history (limit, optional subsystem, optional result, includeSubtasks)
    TaskList !Int !(Maybe Text) !(Maybe Text) !Bool
  | -- | Show single task details (taskId)
    TaskShow !Int64
  | -- | Wait for task to complete (taskId, optional timeout in seconds)
    TaskWait !Int64 !(Maybe Int)
  | -- | Request cancellation of a running task (taskId)
    TaskCancel !Int64
  | -- | Generate shell completion script (bash, zsh, fish)
    Completion !Text
  deriving (Show)
