{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv vm@ subcommands.
module Corvus.Client.Parser.Vm
  ( vmCommandParser
  )
where

import Corvus.Client.Completion
import Corvus.Client.Parser.Utility
import Corvus.Client.Types
import qualified Data.Text as T
import Options.Applicative

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
    <*> switch
      ( long "cloud-init"
          <> help "Enable cloud-init for this VM (required for SSH key injection)"
      )
    <*> switch
      ( long "autostart"
          <> help "Automatically start this VM when the daemon starts"
      )

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
      ( long "delete-disks"
          <> help "Also delete disks that are exclusively attached to this VM"
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
    <*> waitOptionsParser

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
          (info vmStopCommand (progDesc "Stop a VM (running -> stopped). Use --wait to block until stopped."))
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
