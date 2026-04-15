{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv cloud-init@ subcommands.
module Corvus.Client.Parser.CloudInit
  ( cloudInitCommandParser
  )
where

import Corvus.Client.Completion
import Corvus.Client.Types
import qualified Data.Text as T
import Options.Applicative

-- | Parser for cloud-init generate
cloudInitGenerateCommand :: Parser Command
cloudInitGenerateCommand =
  CloudInitGenerate
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )

cloudInitSetCommand :: Parser Command
cloudInitSetCommand =
  CloudInitSet
    <$> argument str (metavar "VM" <> help "VM name or ID")
    <*> optional
      ( argument
          str
          ( metavar "FILE"
              <> help "Path to cloud-init config YAML file (omit to open $EDITOR on a skeleton)"
          )
      )

cloudInitEditCommand :: Parser Command
cloudInitEditCommand =
  CloudInitEdit
    <$> argument str (metavar "VM" <> help "VM name or ID")

cloudInitShowCommand :: Parser Command
cloudInitShowCommand =
  CloudInitShow
    <$> argument str (metavar "VM" <> help "VM name or ID")

cloudInitDeleteCommand :: Parser Command
cloudInitDeleteCommand =
  CloudInitDelete
    <$> argument str (metavar "VM" <> help "VM name or ID")

-- | Parser for cloud-init subcommands
cloudInitCommandParser :: Parser Command
cloudInitCommandParser =
  subparser
    ( command
        "generate"
        (info cloudInitGenerateCommand (progDesc "Generate/regenerate cloud-init ISO for a VM"))
        <> command
          "set"
          (info cloudInitSetCommand (progDesc "Set cloud-init config from a YAML file, or open $EDITOR on a skeleton"))
        <> command
          "edit"
          (info cloudInitEditCommand (progDesc "Fetch cloud-init config, open $EDITOR, and upload the edited YAML"))
        <> command
          "show"
          (info cloudInitShowCommand (progDesc "Show cloud-init config for a VM"))
        <> command
          "delete"
          (info cloudInitDeleteCommand (progDesc "Delete custom cloud-init config (revert to defaults)"))
    )
