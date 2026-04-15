{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv snapshot@ subcommands.
module Corvus.Client.Parser.Snapshot
  ( snapshotCommandParser
  )
where

import Corvus.Client.Completion
import Corvus.Client.Types
import qualified Data.Text as T
import Options.Applicative

-- | Parser for snapshot create
snapshotCreateCommand :: Parser Command
snapshotCreateCommand =
  SnapshotCreate
    <$> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the disk image"
          <> completer diskCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the snapshot"
      )

-- | Parser for snapshot delete
snapshotDeleteCommand :: Parser Command
snapshotDeleteCommand =
  SnapshotDelete
    <$> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the disk image"
          <> completer diskCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "SNAPSHOT"
          <> help "Name or ID of the snapshot to delete"
      )

-- | Parser for snapshot rollback
snapshotRollbackCommand :: Parser Command
snapshotRollbackCommand =
  SnapshotRollback
    <$> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the disk image"
          <> completer diskCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "SNAPSHOT"
          <> help "Name or ID of the snapshot to rollback to"
      )

-- | Parser for snapshot merge
snapshotMergeCommand :: Parser Command
snapshotMergeCommand =
  SnapshotMerge
    <$> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the disk image"
          <> completer diskCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "SNAPSHOT"
          <> help "Name or ID of the snapshot to merge"
      )

-- | Parser for snapshot list
snapshotListCommand :: Parser Command
snapshotListCommand =
  SnapshotList
    <$> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the disk image"
          <> completer diskCompleter
      )

-- | Parser for all snapshot subcommands
snapshotCommandParser :: Parser Command
snapshotCommandParser =
  subparser
    ( command
        "create"
        (info snapshotCreateCommand (progDesc "Create a snapshot (qcow2 only)"))
        <> command
          "delete"
          (info snapshotDeleteCommand (progDesc "Delete a snapshot"))
        <> command
          "rollback"
          (info snapshotRollbackCommand (progDesc "Rollback to a snapshot (VM must be stopped)"))
        <> command
          "merge"
          (info snapshotMergeCommand (progDesc "Merge a snapshot (VM must be stopped)"))
        <> command
          "list"
          (info snapshotListCommand (progDesc "List snapshots for a disk"))
    )
