{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv template@ subcommands.
module Corvus.Client.Parser.Template
  ( templateCommandParser
  )
where

import Corvus.Client.Completion
import Corvus.Client.Types
import qualified Data.Text as T
import Options.Applicative

-- | Parser for template create. FILE is optional; when omitted the client
-- opens $EDITOR on a skeleton template and uses the edited contents.
templateCreateCommand :: Parser Command
templateCreateCommand =
  TemplateCreate
    <$> optional
      ( argument
          str
          ( metavar "FILE"
              <> help "Path to the YAML template file (omit to open $EDITOR on a skeleton)"
              <> action "file"
          )
      )

-- | Parser for template edit
templateEditCommand :: Parser Command
templateEditCommand =
  TemplateEdit
    <$> argument
      (T.pack <$> str)
      ( metavar "TEMPLATE"
          <> help "Name or ID of the template to edit in $EDITOR"
          <> completer templateCompleter
      )

-- | Parser for template delete
templateDeleteCommand :: Parser Command
templateDeleteCommand =
  TemplateDelete
    <$> argument
      (T.pack <$> str)
      ( metavar "TEMPLATE"
          <> help "Name or ID of the template to delete"
          <> completer templateCompleter
      )

-- | Parser for template list
templateListCommand :: Parser Command
templateListCommand = pure TemplateList

-- | Parser for template show
templateShowCommand :: Parser Command
templateShowCommand =
  TemplateShow
    <$> argument
      (T.pack <$> str)
      ( metavar "TEMPLATE"
          <> help "Name or ID of the template to show"
          <> completer templateCompleter
      )

-- | Parser for template instantiate
templateInstantiateCommand :: Parser Command
templateInstantiateCommand =
  TemplateInstantiate
    <$> argument
      (T.pack <$> str)
      ( metavar "TEMPLATE"
          <> help "Name or ID of the template to instantiate"
          <> completer templateCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "VM_NAME"
          <> help "Name for the new VM"
      )

-- | Parser for all template subcommands
templateCommandParser :: Parser Command
templateCommandParser =
  subparser
    ( command
        "create"
        (info templateCreateCommand (progDesc "Create a template from a YAML file, or open $EDITOR on a skeleton if no file is given"))
        <> command
          "edit"
          (info templateEditCommand (progDesc "Fetch a template, open $EDITOR on it, and upload the edited YAML"))
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
