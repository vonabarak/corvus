{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv ssh-key@ subcommands.
module Corvus.Client.Parser.SshKey
  ( sshKeyCommandParser
  )
where

import Corvus.Client.Completion
import Corvus.Client.Types
import qualified Data.Text as T
import Options.Applicative

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
      (T.pack <$> str)
      ( metavar "KEY"
          <> help "Name or ID of the SSH key to delete"
          <> completer sshKeyCompleter
      )

-- | Parser for ssh-key list
sshKeyListCommand :: Parser Command
sshKeyListCommand = pure SshKeyList

-- | Parser for ssh-key attach
sshKeyAttachCommand :: Parser Command
sshKeyAttachCommand =
  SshKeyAttach
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "KEY"
          <> help "Name or ID of the SSH key to attach"
          <> completer sshKeyCompleter
      )

-- | Parser for ssh-key detach
sshKeyDetachCommand :: Parser Command
sshKeyDetachCommand =
  SshKeyDetach
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "KEY"
          <> help "Name or ID of the SSH key to detach"
          <> completer sshKeyCompleter
      )

-- | Parser for ssh-key list-vm
sshKeyListVmCommand :: Parser Command
sshKeyListVmCommand =
  SshKeyListForVm
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
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
