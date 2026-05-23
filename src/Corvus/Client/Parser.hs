{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Command-line argument parsing for the Corvus client.
--
-- Per-subsystem parsers live in "Corvus.Client.Parser.*"; this module
-- composes them via 'subparser' and defines the global options.
module Corvus.Client.Parser
  ( -- * Parsers
    optionsParser
  , optsInfo

    -- * Env-var-derived defaults
  , ClientDefaults (..)
  , readClientDefaults
  )
where

import Corvus.Client.Parser.Apply (applyCommand)
import Corvus.Client.Parser.Build (buildCommand)
import Corvus.Client.Parser.CloudInit (cloudInitCommandParser)
import Corvus.Client.Parser.Disk (diskCommandParser)
import Corvus.Client.Parser.NetIf (netIfCommandParser)
import Corvus.Client.Parser.Network (networkCommandParser)
import Corvus.Client.Parser.Node (nodeCommandParser)
import Corvus.Client.Parser.SharedDir (sharedDirCommandParser)
import Corvus.Client.Parser.Snapshot (snapshotCommandParser)
import Corvus.Client.Parser.SshKey (sshKeyCommandParser)
import Corvus.Client.Parser.Task (taskCommandParser)
import Corvus.Client.Parser.Template (templateCommandParser)
import Corvus.Client.Parser.Vm (vmCommandParser)
import Corvus.Client.Types
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Options.Applicative
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

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
          "node"
          (info nodeCommandParser (progDesc "Multi-node orchestration commands"))
        <> command
          "cloud-init"
          (info cloudInitCommandParser (progDesc "Cloud-init configuration management"))
        <> command
          "apply"
          (info applyCommand (progDesc "Apply environment from YAML config file"))
        <> command
          "build"
          (info buildCommand (progDesc "Build OS images from a YAML pipeline"))
        <> command
          "completion"
          (info completionCommand (progDesc "Generate shell completion script"))
        <> command
          "task"
          (info taskCommandParser (progDesc "Task history commands"))
    )

-- | Defaults derived from environment variables.
--
-- Resolved once before option parsing; the parser uses these as
-- the 'value' for the corresponding 'option', so explicit CLI
-- flags still win. The env var convention mirrors
-- 'Corvus.Client.Completion.getCompletionAddress' so the shell
-- completer and the main CLI see the same address.
data ClientDefaults = ClientDefaults
  { cdSocket :: !(Maybe FilePath)
  , cdHost :: !String
  , cdPort :: !Int
  }
  deriving (Show)

-- | Resolve env vars into 'ClientDefaults'.
--
-- 'CORVUS_SOCKET' is treated as a "pinned Unix socket": when
-- set, the client uses Unix mode targeting that path (overrides
-- the default TCP host/port unless the caller explicitly
-- restores TCP via '--no-unix' or by passing '--host').
readClientDefaults :: IO ClientDefaults
readClientDefaults = do
  mSocket <- lookupEnv "CORVUS_SOCKET"
  mHost <- lookupEnv "CORVUS_HOST"
  mPort <- lookupEnv "CORVUS_PORT"
  pure
    ClientDefaults
      { cdSocket = mSocket
      , cdHost = fromMaybe "127.0.0.1" mHost
      , cdPort = fromMaybe 9876 (mPort >>= readMaybe)
      }

-- | Parser for global options, parameterised by 'ClientDefaults'
-- so the env-var-derived defaults can flow into the 'value'
-- slots without polluting the parser with 'IO'.
optionsParser :: ClientDefaults -> Parser Options
optionsParser ClientDefaults {..} =
  Options
    <$> optional
      ( strOption
          ( long "socket"
              <> short 's'
              <> metavar "PATH"
              <> maybe mempty value cdSocket
              <> help "Unix socket path (default: $XDG_RUNTIME_DIR/corvus/corvus.sock; env: CORVUS_SOCKET)"
          )
      )
    <*> switch
      ( long "unix"
          <> help "Connect via Unix socket instead of TCP (uses --socket / CORVUS_SOCKET if set, else the default socket path)"
      )
    <*> strOption
      ( long "host"
          <> short 'H'
          <> metavar "HOST"
          <> value cdHost
          <> help ("TCP host to connect to (default: " <> cdHost <> "; env: CORVUS_HOST)")
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value cdPort
          <> help ("TCP port to connect to (default: " <> show cdPort <> "; env: CORVUS_PORT)")
      )
    <*> outputFormatParser
    <*> bordersParser
    <*> (not <$> switch (long "no-truncate" <> help "Print full values in table cells instead of truncating with ellipsis (useful for piping to awk/sed)"))
    <*> columnsParser
    <*> (not <$> switch (long "no-fit" <> help "Don't shrink table columns to fit the terminal width"))
    <*> switch
      ( long "no-tls"
          <> help "Skip mutual TLS when connecting over TCP (dev / --no-tls daemon only). No-op for Unix-socket connections."
      )
    <*> optional
      ( strOption
          ( long "tls-cert-dir"
              <> metavar "DIR"
              <> help "Directory containing ca.crt + corvus-client.crt + corvus-client.key (default: $XDG_CONFIG_HOME/corvus)"
          )
      )
    <*> commandParser

-- | Parser for --borders and --no-borders flags.
bordersParser :: Parser BorderStyleOpt
bordersParser =
  option
    readBorderStyle
    ( long "borders"
        <> metavar "STYLE"
        <> value BordersUnicodeOpt
        <> help "Table border style: unicode, ascii, none (default: unicode)"
        <> completeWith ["unicode", "ascii", "none"]
    )
    <|> flag' BordersNoneOpt (long "no-borders" <> help "Disable table borders (shorthand for --borders=none)")

-- | Reader for border style values
readBorderStyle :: ReadM BorderStyleOpt
readBorderStyle = eitherReader $ \s -> case map toLower s of
  "unicode" -> Right BordersUnicodeOpt
  "ascii" -> Right BordersAsciiOpt
  "none" -> Right BordersNoneOpt
  _ -> Left $ "Unknown border style: " ++ s ++ " (use unicode, ascii, or none)"

-- | Parser for --columns flag.
columnsParser :: Parser [String]
columnsParser =
  option
    (splitOn ',' <$> str)
    ( long "columns"
        <> metavar "COLS"
        <> value []
        <> help "Comma-separated column names to show in list output (e.g. --columns id,name). Default: all columns."
    )

-- | Split a string on a delimiter, filtering out empties.
splitOn :: Char -> String -> [String]
splitOn delim s = filter (not . null) (go s)
  where
    go [] = [""]
    go (c : rest)
      | c == delim = "" : go rest
      | otherwise = case go rest of
          (h : t) -> (c : h) : t
          [] -> [[c]]

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

-- | Full parser with info, parameterised by the env-derived
-- 'ClientDefaults' so the help text shows the resolved defaults.
optsInfo :: ClientDefaults -> ParserInfo Options
optsInfo defs =
  info
    (optionsParser defs <**> helper)
    ( fullDesc
        <> progDesc "Corvus client - interact with the corvus daemon"
        <> header "crv - corvus client for VM management"
    )
