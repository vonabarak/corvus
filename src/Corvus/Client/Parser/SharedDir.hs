{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv shared-dir@ subcommands.
module Corvus.Client.Parser.SharedDir
  ( sharedDirCommandParser
  )
where

import Corvus.Client.Completion
import Corvus.Client.Types
import qualified Data.Text as T
import Options.Applicative

-- | Parser for shared-dir add
sharedDirAddCommand :: Parser Command
sharedDirAddCommand =
  SharedDirAdd
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "PATH"
          <> help "Host path to the directory to share"
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "TAG"
          <> help "Unique tag for the shared directory (used as mount tag in VM)"
      )
    <*> strOption
      ( long "cache"
          <> metavar "CACHE_TYPE"
          <> value "auto"
          <> showDefault
          <> help "Cache type: always, auto, never"
          <> completeWith ["always", "auto", "never"]
      )
    <*> switch
      ( long "read-only"
          <> help "Mount the shared directory as read-only"
      )

-- | Parser for shared-dir remove
sharedDirRemoveCommand :: Parser Command
sharedDirRemoveCommand =
  SharedDirRemove
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "SHARED_DIR"
          <> help "Tag or ID of the shared directory to remove"
      )

-- | Parser for shared-dir list
sharedDirListCommand :: Parser Command
sharedDirListCommand =
  SharedDirList
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )

-- | Parser for all shared-dir subcommands
sharedDirCommandParser :: Parser Command
sharedDirCommandParser =
  subparser
    ( command
        "add"
        (info sharedDirAddCommand (progDesc "Add a shared directory to a VM"))
        <> command
          "remove"
          (info sharedDirRemoveCommand (progDesc "Remove a shared directory from a VM"))
        <> command
          "list"
          (info sharedDirListCommand (progDesc "List shared directories for a VM"))
    )
