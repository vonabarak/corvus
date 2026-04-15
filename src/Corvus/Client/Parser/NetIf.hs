{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv net-if@ subcommands.
module Corvus.Client.Parser.NetIf
  ( netIfCommandParser
  )
where

import Corvus.Client.Completion
import Corvus.Client.Types
import qualified Data.Text as T
import Options.Applicative

-- | Parser for net-if add
netIfAddCommand :: Parser Command
netIfAddCommand =
  NetIfAdd
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )
    <*> strOption
      ( long "type"
          <> short 't'
          <> metavar "TYPE"
          <> value "user"
          <> showDefault
          <> help "Interface type: user, tap, bridge, macvtap, vde, managed"
          <> completeWith ["user", "tap", "bridge", "macvtap", "vde", "managed"]
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
          (T.pack <$> str)
          ( long "network"
              <> short 'n'
              <> metavar "NETWORK"
              <> help "Name or ID of the virtual network (overrides --type and --host-device)"
              <> completer networkCompleter
          )
      )

-- | Parser for net-if remove
netIfRemoveCommand :: Parser Command
netIfRemoveCommand =
  NetIfRemove
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
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
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
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
