{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv network@ subcommands.
module Corvus.Client.Parser.Network
  ( networkCommandParser
  )
where

import Corvus.Client.Completion
import Corvus.Client.Parser.Utility
import Corvus.Client.Types
import qualified Data.Text as T
import Options.Applicative

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
    <*> switch
      ( long "dhcp"
          <> help "Enable DHCP (starts dnsmasq when network is running)"
      )
    <*> switch
      ( long "nat"
          <> help "Enable NAT (provides internet access via host network)"
      )
    <*> switch
      ( long "autostart"
          <> help "Automatically start this network when the daemon starts"
      )

-- | Parser for network delete
networkDeleteCommand :: Parser Command
networkDeleteCommand =
  NetworkDelete
    <$> argument
      (T.pack <$> str)
      ( metavar "NETWORK"
          <> help "Name or ID of the network to delete"
          <> completer networkCompleter
      )

-- | Parser for network start
networkStartCommand :: Parser Command
networkStartCommand =
  NetworkStart
    <$> argument
      (T.pack <$> str)
      ( metavar "NETWORK"
          <> help "Name or ID of the network to start"
          <> completer networkCompleter
      )

-- | Parser for network stop
networkStopCommand :: Parser Command
networkStopCommand =
  NetworkStop
    <$> argument
      (T.pack <$> str)
      ( metavar "NETWORK"
          <> help "Name or ID of the network to stop"
          <> completer networkCompleter
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
      (T.pack <$> str)
      ( metavar "NETWORK"
          <> help "Name or ID of the network to show"
          <> completer networkCompleter
      )

-- | Parser for network edit
networkEditCommand :: Parser Command
networkEditCommand =
  NetworkEdit
    <$> argument
      (T.pack <$> str)
      ( metavar "NETWORK"
          <> help "Name or ID of the network to edit"
          <> completer networkCompleter
      )
    <*> optional
      ( strOption
          ( long "subnet"
              <> metavar "SUBNET"
              <> help "New subnet in CIDR notation (e.g., 10.0.1.0/24)"
          )
      )
    <*> optional
      ( option
          readBool
          ( long "dhcp"
              <> metavar "BOOL"
              <> help "Enable/disable DHCP (true/false)"
              <> completeWith ["true", "false"]
          )
      )
    <*> optional
      ( option
          readBool
          ( long "nat"
              <> metavar "BOOL"
              <> help "Enable/disable NAT (true/false)"
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
          (info networkStartCommand (progDesc "Start a virtual network namespace"))
        <> command
          "stop"
          (info networkStopCommand (progDesc "Stop a virtual network"))
        <> command
          "list"
          (info networkListCommand (progDesc "List all virtual networks"))
        <> command
          "show"
          (info networkShowCommand (progDesc "Show virtual network details"))
        <> command
          "edit"
          (info networkEditCommand (progDesc "Edit virtual network properties"))
    )
