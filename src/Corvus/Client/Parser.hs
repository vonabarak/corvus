{-# LANGUAGE OverloadedStrings #-}

-- | Command-line argument parsing for the Corvus client.
module Corvus.Client.Parser
  ( -- * Parsers
    optionsParser,
    optsInfo,
  )
where

import Corvus.Client.Types
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

-- | Parser for all VM subcommands
vmCommandParser :: Parser Command
vmCommandParser =
  subparser
    ( command
        "list"
        (info vmListCommand (progDesc "List all VMs"))
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
          "view"
          (info vmViewCommand (progDesc "View VM via SPICE (runs remote-viewer)"))
        <> command
          "monitor"
          (info vmMonitorCommand (progDesc "Connect to VM's HMP monitor (Ctrl+] to exit)"))
    )

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
    <*> commandParser

-- | Full parser with info
optsInfo :: ParserInfo Options
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Corvus client - interact with the corvus daemon"
        <> header "crv - corvus client for VM management"
    )
