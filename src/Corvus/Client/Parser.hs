{-# LANGUAGE OverloadedStrings #-}

-- | Command-line argument parsing for the Corvus client.
--
-- Per-subsystem parsers live in "Corvus.Client.Parser.*"; this module
-- composes them via 'subparser' and defines the global options.
module Corvus.Client.Parser
  ( -- * Parsers
    optionsParser
  , optsInfo
  )
where

import Corvus.Client.Parser.Apply (applyCommand)
import Corvus.Client.Parser.CloudInit (cloudInitCommandParser)
import Corvus.Client.Parser.Disk (diskCommandParser)
import Corvus.Client.Parser.NetIf (netIfCommandParser)
import Corvus.Client.Parser.Network (networkCommandParser)
import Corvus.Client.Parser.SharedDir (sharedDirCommandParser)
import Corvus.Client.Parser.Snapshot (snapshotCommandParser)
import Corvus.Client.Parser.SshKey (sshKeyCommandParser)
import Corvus.Client.Parser.Task (taskCommandParser)
import Corvus.Client.Parser.Template (templateCommandParser)
import Corvus.Client.Parser.Vm (vmCommandParser)
import Corvus.Client.Types
import Data.Char (toLower)
import qualified Data.Text as T
import Options.Applicative

--------------------------------------------------------------------------------
-- Misc Top-level Command Parsers
--------------------------------------------------------------------------------

-- | Parser for the ping command
pingCommand :: Parser Command
pingCommand = pure Ping

-- | Parser for the status command
statusCommand :: Parser Command
statusCommand = pure Status

-- | Parser for the shutdown command
shutdownCommand :: Parser Command
shutdownCommand = pure Shutdown

-- | Generate shell completion script
completionCommand :: Parser Command
completionCommand =
  Completion
    <$> argument
      (T.pack <$> str)
      ( metavar "SHELL"
          <> help "Shell to generate completion for (bash, zsh, fish)"
          <> completeWith ["bash", "zsh", "fish"]
      )

-- | Parser for namespace exec command
namespaceExecCommand :: Parser Command
namespaceExecCommand =
  NamespaceExec
    <$> many
      ( argument
          str
          ( metavar "COMMAND..."
              <> help "Command to run (default: $SHELL or /bin/sh)"
          )
      )

--------------------------------------------------------------------------------
-- Top-level dispatch
--------------------------------------------------------------------------------

-- | Parser for all top-level commands.
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
        <> command
          "cloud-init"
          (info cloudInitCommandParser (progDesc "Cloud-init configuration management"))
        <> command
          "apply"
          (info applyCommand (progDesc "Apply environment from YAML config file"))
        <> command
          "completion"
          (info completionCommand (progDesc "Generate shell completion script"))
        <> command
          "task"
          (info taskCommandParser (progDesc "Task history commands"))
        <> command
          "ns"
          (info namespaceExecCommand (progDesc "Run command in network namespace (default: interactive shell)"))
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
        <> completeWith ["text", "json", "yaml"]
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
