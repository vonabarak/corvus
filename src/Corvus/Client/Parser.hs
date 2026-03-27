{-# LANGUAGE OverloadedStrings #-}

-- | Command-line argument parsing for the Corvus client.
module Corvus.Client.Parser
  ( -- * Parsers
    optionsParser
  , optsInfo
  )
where

import Corvus.Client.Types
import Data.Char (toLower)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

-- | Parser for the ping command
pingCommand :: Parser Command
pingCommand = pure Ping

-- | Parser for the status command
statusCommand :: Parser Command
statusCommand = pure Status

-- | Parser for the shutdown command
shutdownCommand :: Parser Command
shutdownCommand = pure Shutdown

-- | Parser for vm list
vmListCommand :: Parser Command
vmListCommand = pure VmList

-- | Parser for vm create
vmCreateCommand :: Parser Command
vmCreateCommand =
  VmCreate
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the new VM"
      )
    <*> option
      auto
      ( long "cpus"
          <> short 'c'
          <> metavar "COUNT"
          <> value 1
          <> showDefault
          <> help "Number of CPU cores"
      )
    <*> option
      auto
      ( long "ram"
          <> short 'm'
          <> metavar "MB"
          <> value 1024
          <> showDefault
          <> help "Amount of RAM in MB"
      )
    <*> optional
      ( strOption
          ( long "description"
              <> short 'd'
              <> metavar "TEXT"
              <> help "Optional VM description"
          )
      )
    <*> switch
      ( long "headless"
          <> help "Create VM without graphics (serial console only)"
      )
    <*> switch
      ( long "guest-agent"
          <> help "Enable QEMU guest agent for this VM"
      )

-- | Parser for vm delete
vmDeleteCommand :: Parser Command
vmDeleteCommand =
  VmDelete
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM to delete"
      )

-- | Parser for vm show
vmShowCommand :: Parser Command
vmShowCommand =
  VmShow
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM to show"
      )

-- | Parser for vm start
vmStartCommand :: Parser Command
vmStartCommand =
  VmStart
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM to start"
      )

-- | Parser for vm stop
vmStopCommand :: Parser Command
vmStopCommand =
  VmStop
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM to stop"
      )

-- | Parser for vm pause
vmPauseCommand :: Parser Command
vmPauseCommand =
  VmPause
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM to pause"
      )

-- | Parser for vm reset
vmResetCommand :: Parser Command
vmResetCommand =
  VmReset
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM to reset"
      )

-- | Parser for vm edit
vmEditCommand :: Parser Command
vmEditCommand =
  VmEdit
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM to edit"
      )
    <*> optional
      ( option
          auto
          ( long "cpus"
              <> short 'c'
              <> metavar "COUNT"
              <> help "New number of CPU cores"
          )
      )
    <*> optional
      ( option
          auto
          ( long "ram"
              <> short 'm'
              <> metavar "MB"
              <> help "New amount of RAM in MB"
          )
      )
    <*> optional
      ( strOption
          ( long "description"
              <> short 'd'
              <> metavar "TEXT"
              <> help "New VM description"
          )
      )
    <*> optional
      ( option
          readBool
          ( long "headless"
              <> metavar "BOOL"
              <> help "Set headless mode (true/false)"
          )
      )
    <*> optional
      ( option
          readBool
          ( long "guest-agent"
              <> metavar "BOOL"
              <> help "Enable/disable QEMU guest agent (true/false)"
          )
      )

-- | Reader for boolean values
readBool :: ReadM Bool
readBool = eitherReader $ \s -> case map toLower s of
  "true" -> Right True
  "false" -> Right False
  "yes" -> Right True
  "no" -> Right False
  "1" -> Right True
  "0" -> Right False
  _ -> Left $ "Invalid boolean: " ++ s ++ " (use true/false)"

-- | Parser for vm view
vmViewCommand :: Parser Command
vmViewCommand =
  VmView
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM to view via SPICE"
      )

-- | Parser for vm monitor
vmMonitorCommand :: Parser Command
vmMonitorCommand =
  VmMonitor
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM to connect to HMP monitor"
      )

-- | Parser for vm exec
vmExecCommand :: Parser Command
vmExecCommand =
  VmExec
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM to execute command in"
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "COMMAND"
          <> help "Command to execute inside the VM"
      )

-- | Parser for all VM subcommands
vmCommandParser :: Parser Command
vmCommandParser =
  subparser
    ( command
        "list"
        (info vmListCommand (progDesc "List all VMs"))
        <> command
          "create"
          (info vmCreateCommand (progDesc "Create a new VM"))
        <> command
          "delete"
          (info vmDeleteCommand (progDesc "Delete a VM"))
        <> command
          "show"
          (info vmShowCommand (progDesc "Show VM details"))
        <> command
          "start"
          (info vmStartCommand (progDesc "Start a VM (stopped/paused -> running)"))
        <> command
          "stop"
          (info vmStopCommand (progDesc "Stop a VM (running -> stopped)"))
        <> command
          "pause"
          (info vmPauseCommand (progDesc "Pause a VM (running -> paused)"))
        <> command
          "reset"
          (info vmResetCommand (progDesc "Reset a VM to stopped state (any -> stopped)"))
        <> command
          "edit"
          (info vmEditCommand (progDesc "Edit VM properties (VM must be stopped)"))
        <> command
          "view"
          (info vmViewCommand (progDesc "View VM via SPICE (runs remote-viewer)"))
        <> command
          "monitor"
          (info vmMonitorCommand (progDesc "Connect to VM's HMP monitor (Ctrl+] to exit)"))
        <> command
          "exec"
          (info vmExecCommand (progDesc "Execute a command inside a VM via guest agent"))
    )

--------------------------------------------------------------------------------
-- Disk Image Command Parsers
--------------------------------------------------------------------------------

-- | Helper to parse size with unit suffix (e.g., "10G", "1024M")
parseSizeWithUnit :: ReadM Int64
parseSizeWithUnit = eitherReader $ \s ->
  case reads s of
    [(n, "")] -> Right n
    [(n, "M")] -> Right n
    [(n, "G")] -> Right (n * 1024)
    [(n, "T")] -> Right (n * 1024 * 1024)
    _ -> Left $ "Invalid size format: " ++ s ++ " (use number with optional M/G/T suffix)"

-- | Parser for disk create
diskCreateCommand :: Parser Command
diskCreateCommand =
  DiskCreate
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the disk image"
      )
    <*> strOption
      ( long "format"
          <> short 'f'
          <> metavar "FORMAT"
          <> value "qcow2"
          <> help "Disk format: qcow2, raw, vmdk, vdi (default: qcow2)"
      )
    <*> option
      parseSizeWithUnit
      ( long "size"
          <> short 's'
          <> metavar "SIZE"
          <> help "Disk size in MB (or with suffix: 10G, 100M)"
      )

-- | Parser for disk delete
diskDeleteCommand :: Parser Command
diskDeleteCommand =
  DiskDelete
    <$> argument
      auto
      ( metavar "DISK_ID"
          <> help "ID of the disk image to delete"
      )

-- | Parser for disk resize
diskResizeCommand :: Parser Command
diskResizeCommand =
  DiskResize
    <$> argument
      auto
      ( metavar "DISK_ID"
          <> help "ID of the disk image to resize"
      )
    <*> option
      parseSizeWithUnit
      ( long "size"
          <> short 's'
          <> metavar "SIZE"
          <> help "New size in MB (or with suffix: 20G, 2048M)"
      )

-- | Parser for disk list
diskListCommand :: Parser Command
diskListCommand = pure DiskList

-- | Parser for disk show
diskShowCommand :: Parser Command
diskShowCommand =
  DiskShow
    <$> argument
      auto
      ( metavar "DISK_ID"
          <> help "ID of the disk image to show"
      )

-- | Parser for disk attach
diskAttachCommand :: Parser Command
diskAttachCommand =
  DiskAttach
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM"
      )
    <*> argument
      auto
      ( metavar "DISK_ID"
          <> help "ID of the disk image to attach"
      )
    <*> strOption
      ( long "interface"
          <> short 'i'
          <> metavar "INTERFACE"
          <> value "virtio"
          <> help "Drive interface: virtio, ide, scsi, sata, nvme (default: virtio)"
      )
    <*> optional
      ( strOption
          ( long "media"
              <> short 'm'
              <> metavar "MEDIA"
              <> help "Media type: disk, cdrom"
          )
      )
    <*> switch
      ( long "read-only"
          <> help "Attach the disk in read-only mode (required for base images with overlays)"
      )
    <*> switch
      ( long "discard"
          <> help "Enable discard support (UNMAP/TRIM) (default: off)"
      )
    <*> strOption
      ( long "cache"
          <> metavar "CACHE_TYPE"
          <> value "writeback"
          <> help "Disk cache type: writeback, none, writethrough, directsync, unsafe (default: writeback)"
      )

-- | Parser for disk detach
diskDetachCommand :: Parser Command
diskDetachCommand =
  DiskDetach
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM"
      )
    <*> argument
      auto
      ( metavar "DRIVE_ID"
          <> help "ID of the drive to detach"
      )

-- | Parser for disk import
diskImportCommand :: Parser Command
diskImportCommand =
  DiskImport
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the disk image"
      )
    <*> argument
      str
      ( metavar "PATH"
          <> help "Path to existing disk image file"
      )
    <*> optional
      ( strOption
          ( long "format"
              <> short 'f'
              <> metavar "FORMAT"
              <> help "Disk format: qcow2, raw (auto-detected from extension if not specified)"
          )
      )

-- | Parser for disk overlay
diskOverlayCommand :: Parser Command
diskOverlayCommand =
  DiskCreateOverlay
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the overlay disk image"
      )
    <*> argument
      auto
      ( metavar "BASE_DISK_ID"
          <> help "ID of the base disk image to overlay"
      )
    <*> optional
      ( strOption
          ( long "path"
              <> short 'p'
              <> metavar "DIR"
              <> help "Directory for the overlay file (default: base images path)"
          )
      )

-- | Parser for disk clone
diskCloneCommand :: Parser Command
diskCloneCommand =
  DiskClone
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the cloned disk image"
      )
    <*> argument
      auto
      ( metavar "BASE_DISK_ID"
          <> help "ID of the base disk image to clone"
      )
    <*> optional
      ( strOption
          ( long "path"
              <> short 'p'
              <> metavar "PATH"
              <> help "Optional destination path for the cloned image file"
          )
      )

-- | Parser for all disk subcommands
diskCommandParser :: Parser Command
diskCommandParser =
  subparser
    ( command
        "create"
        (info diskCreateCommand (progDesc "Create a new disk image"))
        <> command
          "overlay"
          (info diskOverlayCommand (progDesc "Create a qcow2 overlay backed by an existing disk"))
        <> command
          "import"
          (info diskImportCommand (progDesc "Import an existing disk image"))
        <> command
          "delete"
          (info diskDeleteCommand (progDesc "Delete a disk image"))
        <> command
          "resize"
          (info diskResizeCommand (progDesc "Resize a disk image"))
        <> command
          "list"
          (info diskListCommand (progDesc "List all disk images"))
        <> command
          "show"
          (info diskShowCommand (progDesc "Show disk image details"))
        <> command
          "clone"
          (info diskCloneCommand (progDesc "Clone a disk image"))
        <> command
          "attach"
          (info diskAttachCommand (progDesc "Attach a disk to a VM"))
        <> command
          "detach"
          (info diskDetachCommand (progDesc "Detach a disk from a VM"))
    )

--------------------------------------------------------------------------------
-- Snapshot Command Parsers
--------------------------------------------------------------------------------

-- | Parser for snapshot create
snapshotCreateCommand :: Parser Command
snapshotCreateCommand =
  SnapshotCreate
    <$> argument
      auto
      ( metavar "DISK_ID"
          <> help "ID of the disk image"
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the snapshot"
      )

-- | Parser for snapshot delete
snapshotDeleteCommand :: Parser Command
snapshotDeleteCommand =
  SnapshotDelete
    <$> argument
      auto
      ( metavar "DISK_ID"
          <> help "ID of the disk image"
      )
    <*> argument
      auto
      ( metavar "SNAPSHOT_ID"
          <> help "ID of the snapshot to delete"
      )

-- | Parser for snapshot rollback
snapshotRollbackCommand :: Parser Command
snapshotRollbackCommand =
  SnapshotRollback
    <$> argument
      auto
      ( metavar "DISK_ID"
          <> help "ID of the disk image"
      )
    <*> argument
      auto
      ( metavar "SNAPSHOT_ID"
          <> help "ID of the snapshot to rollback to"
      )

-- | Parser for snapshot merge
snapshotMergeCommand :: Parser Command
snapshotMergeCommand =
  SnapshotMerge
    <$> argument
      auto
      ( metavar "DISK_ID"
          <> help "ID of the disk image"
      )
    <*> argument
      auto
      ( metavar "SNAPSHOT_ID"
          <> help "ID of the snapshot to merge"
      )

-- | Parser for snapshot list
snapshotListCommand :: Parser Command
snapshotListCommand =
  SnapshotList
    <$> argument
      auto
      ( metavar "DISK_ID"
          <> help "ID of the disk image"
      )

-- | Parser for all snapshot subcommands
snapshotCommandParser :: Parser Command
snapshotCommandParser =
  subparser
    ( command
        "create"
        (info snapshotCreateCommand (progDesc "Create a snapshot (qcow2 only)"))
        <> command
          "delete"
          (info snapshotDeleteCommand (progDesc "Delete a snapshot"))
        <> command
          "rollback"
          (info snapshotRollbackCommand (progDesc "Rollback to a snapshot (VM must be stopped)"))
        <> command
          "merge"
          (info snapshotMergeCommand (progDesc "Merge a snapshot (VM must be stopped)"))
        <> command
          "list"
          (info snapshotListCommand (progDesc "List snapshots for a disk"))
    )

--------------------------------------------------------------------------------
-- SSH Key Command Parsers
--------------------------------------------------------------------------------

-- | Parser for ssh-key create
sshKeyCreateCommand :: Parser Command
sshKeyCreateCommand =
  SshKeyCreate
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the SSH key"
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "PUBLIC_KEY"
          <> help "SSH public key content"
      )

-- | Parser for ssh-key delete
sshKeyDeleteCommand :: Parser Command
sshKeyDeleteCommand =
  SshKeyDelete
    <$> argument
      auto
      ( metavar "KEY_ID"
          <> help "ID of the SSH key to delete"
      )

-- | Parser for ssh-key list
sshKeyListCommand :: Parser Command
sshKeyListCommand = pure SshKeyList

-- | Parser for ssh-key attach
sshKeyAttachCommand :: Parser Command
sshKeyAttachCommand =
  SshKeyAttach
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM"
      )
    <*> argument
      auto
      ( metavar "KEY_ID"
          <> help "ID of the SSH key to attach"
      )

-- | Parser for ssh-key detach
sshKeyDetachCommand :: Parser Command
sshKeyDetachCommand =
  SshKeyDetach
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM"
      )
    <*> argument
      auto
      ( metavar "KEY_ID"
          <> help "ID of the SSH key to detach"
      )

-- | Parser for ssh-key list-vm
sshKeyListVmCommand :: Parser Command
sshKeyListVmCommand =
  SshKeyListForVm
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM"
      )

-- | Parser for all ssh-key subcommands
sshKeyCommandParser :: Parser Command
sshKeyCommandParser =
  subparser
    ( command
        "create"
        (info sshKeyCreateCommand (progDesc "Create a new SSH key"))
        <> command
          "delete"
          (info sshKeyDeleteCommand (progDesc "Delete an SSH key"))
        <> command
          "list"
          (info sshKeyListCommand (progDesc "List all SSH keys"))
        <> command
          "attach"
          (info sshKeyAttachCommand (progDesc "Attach an SSH key to a VM"))
        <> command
          "detach"
          (info sshKeyDetachCommand (progDesc "Detach an SSH key from a VM"))
        <> command
          "list-vm"
          (info sshKeyListVmCommand (progDesc "List SSH keys attached to a VM"))
    )

--------------------------------------------------------------------------------
-- Shared Directory Command Parsers
--------------------------------------------------------------------------------

-- | Parser for shared-dir add
sharedDirAddCommand :: Parser Command
sharedDirAddCommand =
  SharedDirAdd
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM"
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "PATH"
          <> help "Host path to the directory to share"
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "TAG"
          <> help "Unique tag for the shared directory (used as mount tag in VM)"
      )
    <*> strOption
      ( long "cache"
          <> metavar "CACHE_TYPE"
          <> value "auto"
          <> showDefault
          <> help "Cache type: always, auto, never"
      )
    <*> switch
      ( long "read-only"
          <> help "Mount the shared directory as read-only"
      )

-- | Parser for shared-dir remove
sharedDirRemoveCommand :: Parser Command
sharedDirRemoveCommand =
  SharedDirRemove
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM"
      )
    <*> argument
      auto
      ( metavar "SHARED_DIR_ID"
          <> help "ID of the shared directory to remove"
      )

-- | Parser for shared-dir list
sharedDirListCommand :: Parser Command
sharedDirListCommand =
  SharedDirList
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM"
      )

-- | Parser for all shared-dir subcommands
sharedDirCommandParser :: Parser Command
sharedDirCommandParser =
  subparser
    ( command
        "add"
        (info sharedDirAddCommand (progDesc "Add a shared directory to a VM"))
        <> command
          "remove"
          (info sharedDirRemoveCommand (progDesc "Remove a shared directory from a VM"))
        <> command
          "list"
          (info sharedDirListCommand (progDesc "List shared directories for a VM"))
    )

--------------------------------------------------------------------------------
-- Network Interface Command Parsers
--------------------------------------------------------------------------------

-- | Parser for net-if add
netIfAddCommand :: Parser Command
netIfAddCommand =
  NetIfAdd
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM"
      )
    <*> strOption
      ( long "type"
          <> short 't'
          <> metavar "TYPE"
          <> value "user"
          <> showDefault
          <> help "Interface type: user, tap, bridge, macvtap, vde"
      )
    <*> strOption
      ( long "host-device"
          <> short 'd'
          <> metavar "DEVICE"
          <> value ""
          <> help "Host device name (for tap/bridge/macvtap)"
      )
    <*> optional
      ( strOption
          ( long "mac"
              <> metavar "MAC"
              <> help "MAC address (auto-generated if not specified)"
          )
      )
    <*> optional
      ( option
          auto
          ( long "network"
              <> short 'n'
              <> metavar "NETWORK_ID"
              <> help "Virtual network ID (overrides --type and --host-device)"
          )
      )

-- | Parser for net-if remove
netIfRemoveCommand :: Parser Command
netIfRemoveCommand =
  NetIfRemove
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM"
      )
    <*> argument
      auto
      ( metavar "NETIF_ID"
          <> help "ID of the network interface to remove"
      )

-- | Parser for net-if list
netIfListCommand :: Parser Command
netIfListCommand =
  NetIfList
    <$> argument
      auto
      ( metavar "VM_ID"
          <> help "ID of the VM"
      )

-- | Parser for template create
templateCreateCommand :: Parser Command
templateCreateCommand =
  TemplateCreate
    <$> argument
      str
      ( metavar "FILE"
          <> help "Path to the YAML template file"
      )

-- | Parser for template delete
templateDeleteCommand :: Parser Command
templateDeleteCommand =
  TemplateDelete
    <$> argument
      auto
      ( metavar "TEMPLATE_ID"
          <> help "ID of the template to delete"
      )

-- | Parser for template list
templateListCommand :: Parser Command
templateListCommand = pure TemplateList

-- | Parser for template show
templateShowCommand :: Parser Command
templateShowCommand =
  TemplateShow
    <$> argument
      auto
      ( metavar "TEMPLATE_ID"
          <> help "ID of the template to show"
      )

-- | Parser for template instantiate
templateInstantiateCommand :: Parser Command
templateInstantiateCommand =
  TemplateInstantiate
    <$> argument
      auto
      ( metavar "TEMPLATE_ID"
          <> help "ID of the template to instantiate"
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "VM_NAME"
          <> help "Name for the new VM"
      )

-- | Parser for all net-if subcommands
netIfCommandParser :: Parser Command
netIfCommandParser =
  subparser
    ( command
        "add"
        (info netIfAddCommand (progDesc "Add a network interface to a VM"))
        <> command
          "remove"
          (info netIfRemoveCommand (progDesc "Remove a network interface from a VM"))
        <> command
          "list"
          (info netIfListCommand (progDesc "List network interfaces for a VM"))
    )

-- | Parser for all template subcommands
templateCommandParser :: Parser Command
templateCommandParser =
  subparser
    ( command
        "create"
        (info templateCreateCommand (progDesc "Create a template from a YAML file"))
        <> command
          "delete"
          (info templateDeleteCommand (progDesc "Delete a template"))
        <> command
          "list"
          (info templateListCommand (progDesc "List all templates"))
        <> command
          "show"
          (info templateShowCommand (progDesc "Show template details"))
        <> command
          "instantiate"
          (info templateInstantiateCommand (progDesc "Instantiate a VM from a template"))
    )

--------------------------------------------------------------------------------
-- Virtual Network Command Parsers
--------------------------------------------------------------------------------

-- | Parser for network create
networkCreateCommand :: Parser Command
networkCreateCommand =
  NetworkCreate
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the virtual network"
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "SUBNET"
          <> help "IPv4 subnet in CIDR notation (e.g., 10.0.1.0/24)"
          <> value ""
      )

-- | Parser for network delete
networkDeleteCommand :: Parser Command
networkDeleteCommand =
  NetworkDelete
    <$> argument
      auto
      ( metavar "NETWORK_ID"
          <> help "ID of the network to delete"
      )

-- | Parser for network start
networkStartCommand :: Parser Command
networkStartCommand =
  NetworkStart
    <$> argument
      auto
      ( metavar "NETWORK_ID"
          <> help "ID of the network to start"
      )

-- | Parser for network stop
networkStopCommand :: Parser Command
networkStopCommand =
  NetworkStop
    <$> argument
      auto
      ( metavar "NETWORK_ID"
          <> help "ID of the network to stop"
      )
    <*> switch
      ( long "force"
          <> short 'f'
          <> help "Force stop even if running VMs are connected"
      )

-- | Parser for network list
networkListCommand :: Parser Command
networkListCommand = pure NetworkList

-- | Parser for network show
networkShowCommand :: Parser Command
networkShowCommand =
  NetworkShow
    <$> argument
      auto
      ( metavar "NETWORK_ID"
          <> help "ID of the network to show"
      )

-- | Parser for all network subcommands
networkCommandParser :: Parser Command
networkCommandParser =
  subparser
    ( command
        "create"
        (info networkCreateCommand (progDesc "Create a virtual network"))
        <> command
          "delete"
          (info networkDeleteCommand (progDesc "Delete a virtual network"))
        <> command
          "start"
          (info networkStartCommand (progDesc "Start a virtual network (launch vde_switch)"))
        <> command
          "stop"
          (info networkStopCommand (progDesc "Stop a virtual network"))
        <> command
          "list"
          (info networkListCommand (progDesc "List all virtual networks"))
        <> command
          "show"
          (info networkShowCommand (progDesc "Show virtual network details"))
    )

--------------------------------------------------------------------------------
-- Main Command Parser
--------------------------------------------------------------------------------

-- | Parser for all commands
commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "ping"
        (info pingCommand (progDesc "Ping the daemon"))
        <> command
          "status"
          (info statusCommand (progDesc "Get daemon status"))
        <> command
          "shutdown"
          (info shutdownCommand (progDesc "Request daemon shutdown"))
        <> command
          "vm"
          (info vmCommandParser (progDesc "VM management commands"))
        <> command
          "disk"
          (info diskCommandParser (progDesc "Disk image management commands"))
        <> command
          "snapshot"
          (info snapshotCommandParser (progDesc "Snapshot management commands"))
        <> command
          "ssh-key"
          (info sshKeyCommandParser (progDesc "SSH key management commands"))
        <> command
          "net-if"
          (info netIfCommandParser (progDesc "Network interface management commands"))
        <> command
          "shared-dir"
          (info sharedDirCommandParser (progDesc "Shared directory management commands"))
        <> command
          "template"
          (info templateCommandParser (progDesc "Template management commands"))
        <> command
          "network"
          (info networkCommandParser (progDesc "Virtual network management commands"))
    )

-- | Parser for global options
optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional
      ( strOption
          ( long "socket"
              <> short 's'
              <> metavar "PATH"
              <> help "Unix socket path (default: $XDG_RUNTIME_DIR/corvus/corvus.sock)"
          )
      )
    <*> switch
      ( long "tcp"
          <> help "Use TCP instead of Unix socket"
      )
    <*> strOption
      ( long "host"
          <> short 'H'
          <> metavar "HOST"
          <> value "127.0.0.1"
          <> help "Host to connect to when using --tcp (default: 127.0.0.1)"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 9876
          <> help "Port to connect to when using --tcp (default: 9876)"
      )
    <*> outputFormatParser
    <*> commandParser

-- | Parser for output format
outputFormatParser :: Parser OutputFormat
outputFormatParser =
  option
    readOutputFormat
    ( long "output"
        <> short 'o'
        <> metavar "FORMAT"
        <> value TextOutput
        <> help "Output format: text, json, yaml (default: text)"
    )

-- | Reader for output format values
readOutputFormat :: ReadM OutputFormat
readOutputFormat = eitherReader $ \s -> case map toLower s of
  "text" -> Right TextOutput
  "json" -> Right JsonOutput
  "yaml" -> Right YamlOutput
  _ -> Left $ "Unknown output format: " ++ s ++ " (use text, json, or yaml)"

-- | Full parser with info
optsInfo :: ParserInfo Options
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Corvus client - interact with the corvus daemon"
        <> header "crv - corvus client for VM management"
    )
