{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv vm@ subcommands.
module Corvus.Client.Parser.Vm
  ( vmCommandParser
  )
where

import Corvus.Client.Completion
import Corvus.Client.Parser.Utility
import Corvus.Client.Types
import Corvus.Model (EnumText (..), GraphicsAdapter (..))
import Corvus.Size (parseRam)
import qualified Data.Text as T
import Options.Applicative

nonNegativeInt :: ReadM Int
nonNegativeInt =
  eitherReader $ \raw ->
    case reads raw of
      [(n, "")] | n >= 0 -> Right n
      _ -> Left "expected a non-negative integer"

graphicsAdapterReader :: ReadM GraphicsAdapter
graphicsAdapterReader = eitherReader (either (Left . T.unpack) Right . enumFromText . T.pack)

graphicsAdapterOption :: Mod OptionFields GraphicsAdapter -> Parser GraphicsAdapter
graphicsAdapterOption mods =
  option
    graphicsAdapterReader
    (mods <> completeWith (map (T.unpack . snd) (enumMapping :: [(GraphicsAdapter, T.Text)])))

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
    <*> ( T.pack
            <$> strOption
              ( long "node"
                  <> short 'n'
                  <> metavar "NODE"
                  <> value ""
                  <> help "Name or ID of the node to place this VM on (optional; daemon picks via the scheduler when omitted)"
              )
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
      (eitherReader parseRam)
      ( long "ram"
          <> short 'm'
          <> metavar "SIZE"
          <> value 1073741824
          <> showDefault
          <> help "Amount of RAM with a B/K/M/G/T suffix (whole MiB)"
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
    <*> switch
      ( long "tpm"
          <> help "Enable a software-emulated TPM 2.0 device for this VM"
      )
    <*> switch
      ( long "cloud-init"
          <> help "Enable cloud-init for this VM (required for SSH key injection)"
      )
    <*> switch
      ( long "autostart"
          <> help "Automatically start this VM when the daemon starts"
      )
    <*> switch
      ( long "reboot-quirk"
          <> help
            "Enable the reboot quirk: QEMU runs with `-no-reboot` so it \
            \exits on guest reboot, and the agent re-spawns it. Works \
            \around the OVMF firmware second-boot hang \
            \(tianocore/edk2#12441)."
      )
    <*> ( T.pack
            <$> strOption
              ( long "cpu-model"
                  <> metavar "MODEL"
                  <> value "host"
                  <> showDefault
                  <> help
                    "QEMU `-cpu` model. Default `host` exposes the host CPU \
                    \(best perf, NOT migration-safe across non-identical \
                    \hosts). For cross-host migration safety, set a stable \
                    \model such as `qemu64`, `Nehalem`, `Westmere-v3`, or \
                    \`Skylake-Client-v1`."
              )
        )
    <*> graphicsAdapterOption
      ( long "graphics-adapter"
          <> metavar "MODEL"
          <> value GraphicsVirtioVga
          <> showDefaultWith (T.unpack . enumToText)
          <> help "Graphics adapter: virtio-vga, qxl-vga, vga, virtio-gpu-pci, virtio-vga-gl, virtio-gpu-gl-pci"
      )
    <*> (not <$> switch (long "no-vsock" <> help "Do not attach a VirtIO vsock device"))
    <*> (not <$> switch (long "no-balloon" <> help "Do not attach a VirtIO memory balloon device"))
    <*> (not <$> switch (long "no-rng" <> help "Do not attach a VirtIO RNG device"))

-- | Parser for vm delete
vmDeleteCommand :: Parser Command
vmDeleteCommand =
  VmDelete
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to delete"
          <> completer vmCompleter
      )
    <*> switch
      ( long "keep-disks"
          <> help "Do not delete ephemeral disks (cloud-init ISOs, template-instantiated disks) attached to this VM"
      )
    <*> switch
      ( long "force"
          <> help "Hard-reset the VM before deleting it; discards unsaved guest state"
      )

-- | Parser for vm show
vmShowCommand :: Parser Command
vmShowCommand =
  VmShow
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to show"
          <> completer vmCompleter
      )

-- | Parser for vm start
vmStartCommand :: Parser Command
vmStartCommand =
  VmStart
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to start"
          <> completer vmCompleter
      )
    <*> waitOptionsParser

-- | Parser for vm stop
vmStopCommand :: Parser Command
vmStopCommand =
  VmStop
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to stop"
          <> completer vmCompleter
      )
    <*> ( WaitOptions
            <$> switch
              ( long "wait"
                  <> short 'w'
                  <> help "Block until the operation completes"
              )
            <*> optional
              ( option
                  nonNegativeInt
                  ( long "timeout"
                      <> short 't'
                      <> metavar "SECONDS"
                      <> help "Graceful-shutdown window before hard kill (0 = immediate; default 300)"
                  )
              )
        )

-- | Parser for vm pause
vmPauseCommand :: Parser Command
vmPauseCommand =
  VmPause
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to pause"
          <> completer vmCompleter
      )

-- | Set a running guest's RAM target.
vmBalloonCommand :: Parser Command
vmBalloonCommand =
  VmSetBalloon
    <$> argument
      (T.pack <$> str)
      (metavar "VM" <> help "Name or ID of the running VM" <> completer vmCompleter)
    <*> argument
      parseSizeBytes
      (metavar "SIZE" <> help "Guest RAM target with binary B/K/M/G/T suffix (e.g. 768M; 1K = 1024 bytes)")

-- | Parser for vm reset
vmResetCommand :: Parser Command
vmResetCommand =
  VmReset
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to reset"
          <> completer vmCompleter
      )

-- | Parser for vm save
vmSaveCommand :: Parser Command
vmSaveCommand =
  VmSave
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to save"
          <> completer vmCompleter
      )
    <*> waitOptionsParser

-- | Parser for vm edit
vmEditCommand :: Parser Command
vmEditCommand =
  VmEdit
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to edit"
          <> completer vmCompleter
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
          (eitherReader parseRam)
          ( long "ram"
              <> short 'm'
              <> metavar "SIZE"
              <> help "New amount of RAM with a B/K/M/G/T suffix (whole MiB)"
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
              <> completeWith ["true", "false"]
          )
      )
    <*> optional
      ( option
          readBool
          ( long "guest-agent"
              <> metavar "BOOL"
              <> help "Enable/disable QEMU guest agent (true/false)"
              <> completeWith ["true", "false"]
          )
      )
    <*> optional
      ( option
          readBool
          ( long "tpm"
              <> metavar "BOOL"
              <> help "Enable/disable the software-emulated TPM 2.0 device (true/false); disabling erases TPM state"
              <> completeWith ["true", "false"]
          )
      )
    <*> optional
      ( option
          readBool
          ( long "cloud-init"
              <> metavar "BOOL"
              <> help "Enable/disable cloud-init (true/false)"
              <> completeWith ["true", "false"]
          )
      )
    <*> optional
      ( option
          readBool
          ( long "autostart"
              <> metavar "BOOL"
              <> help "Enable/disable autostart (true/false)"
              <> completeWith ["true", "false"]
          )
      )
    <*> optional
      ( option
          readBool
          ( long "reboot-quirk"
              <> metavar "BOOL"
              <> help
                "Enable/disable the reboot-on-exit quirk for OVMF \
                \workarounds (true/false). Takes effect on next start."
              <> completeWith ["true", "false"]
          )
      )
    <*> optional
      ( T.pack
          <$> strOption
            ( long "cpu-model"
                <> metavar "MODEL"
                <> help
                  "New QEMU `-cpu` model (e.g. `host`, `qemu64`, \
                  \`Westmere-v3`). Takes effect on next start."
            )
      )
    <*> optional
      ( graphicsAdapterOption
          ( long "graphics-adapter"
              <> metavar "MODEL"
              <> help "New graphics adapter (requires stopped VM)"
          )
      )
    <*> optional (option readBool (long "vsock" <> metavar "BOOL" <> help "Enable/disable VirtIO vsock (true/false; requires stopped VM)" <> completeWith ["true", "false"]))
    <*> optional (option readBool (long "balloon" <> metavar "BOOL" <> help "Enable/disable VirtIO memory balloon (true/false; requires stopped VM)" <> completeWith ["true", "false"]))
    <*> optional (option readBool (long "rng" <> metavar "BOOL" <> help "Enable/disable VirtIO RNG (true/false; requires stopped VM)" <> completeWith ["true", "false"]))

-- | Parser for vm view
vmViewCommand :: Parser Command
vmViewCommand =
  VmView
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to view via SPICE"
          <> completer vmCompleter
      )

-- | Parser for vm monitor
vmMonitorCommand :: Parser Command
vmMonitorCommand =
  VmMonitor
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to connect to HMP monitor"
          <> completer vmCompleter
      )

-- | Parser for vm exec
vmExecCommand :: Parser Command
vmExecCommand =
  VmExec
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to execute command in"
          <> completer vmCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "COMMAND"
          <> help "Command to execute inside the VM"
      )

-- | Parser for @crv vm snapshot create VM NAME@.
vmSnapshotCreateCommand :: Parser Command
vmSnapshotCreateCommand =
  VmSnapshotCreate
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to snapshot"
          <> completer vmCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the snapshot"
      )

-- | Parser for @crv vm snapshot list VM@.
vmSnapshotListCommand :: Parser Command
vmSnapshotListCommand =
  VmSnapshotList
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )

-- | Parser for @crv vm snapshot rollback VM NAME@.
vmSnapshotRollbackCommand :: Parser Command
vmSnapshotRollbackCommand =
  VmSnapshotRollback
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name of the snapshot to roll back to"
      )

-- | Parser for @crv vm snapshot delete VM NAME@.
vmSnapshotDeleteCommand :: Parser Command
vmSnapshotDeleteCommand =
  VmSnapshotDelete
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name of the snapshot to delete"
      )

-- | Sub-subparser for @crv vm snapshot ...@.
vmSnapshotCommandParser :: Parser Command
vmSnapshotCommandParser =
  subparser
    ( command
        "create"
        ( info
            vmSnapshotCreateCommand
            ( progDesc
                "Create a full-machine snapshot of the VM (all disks + RAM)"
            )
        )
        <> command
          "list"
          (info vmSnapshotListCommand (progDesc "List the VM's full-machine snapshots"))
        <> command
          "rollback"
          ( info
              vmSnapshotRollbackCommand
              ( progDesc
                  "Roll the VM back to a named snapshot (works on running or stopped VMs)"
              )
          )
        <> command
          "delete"
          ( info
              vmSnapshotDeleteCommand
              (progDesc "Delete a VM snapshot (VM must be running)")
          )
    )

-- | Parser for @crv vm migrate@: move a VM to another node without live migration.
vmMigrateCommand :: Parser Command
vmMigrateCommand =
  VmMigrate
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM to migrate"
          <> completer vmCompleter
      )
    <*> strOption
      ( long "to-node"
          <> metavar "NODE"
          <> help "Destination node (name or ID)"
          <> completer nodeCompleter
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
          ( info
              vmStartCommand
              ( progDesc
                  "Start a VM. Cold-boots from stopped, resumes from paused (QMP cont), or restores from saved state (file:incoming + cont)."
              )
          )
        <> command
          "stop"
          (info vmStopCommand (progDesc "Stop a VM (running -> stopped). Use --wait to block until stopped."))
        <> command
          "pause"
          (info vmPauseCommand (progDesc "Pause a VM (running -> paused)"))
        <> command
          "balloon"
          (info vmBalloonCommand (progDesc "Set the running guest RAM target; returns on QEMU acceptance"))
        <> command
          "reset"
          (info vmResetCommand (progDesc "Reset a VM to stopped state (any -> stopped). For saved VMs this also drops the saved-state file."))
        <> command
          "save"
          ( info
              vmSaveCommand
              ( progDesc
                  "Save the VM's running state to disk (running/paused -> saved). Resume later with 'vm start'."
              )
          )
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
        <> command
          "migrate"
          (info vmMigrateCommand (progDesc "Migrate a VM to another node without live memory streaming (agent-to-agent)"))
        <> command
          "snapshot"
          ( info
              vmSnapshotCommandParser
              ( progDesc
                  "VM-scoped full-machine snapshots (all disks + RAM/CPU/device state)"
              )
          )
    )
