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
    <*> option
      (eitherReader parseQuiesceFlag)
      ( long "quiesce"
          <> metavar "auto|require|skip"
          <> value QuiesceFlagAuto
          <> help
            ( "Filesystem quiesce policy for live (running-VM) snapshots."
                ++ " 'auto' (default): freeze guest filesystems via QGA"
                ++ " fsfreeze if the guest agent is reachable, else skip."
                ++ " 'require': fail the snapshot if QGA is unreachable"
                ++ " or fsfreeze errors. 'skip': never freeze."
                ++ " Has no effect on offline (stopped-VM) snapshots."
            )
      )
    <*> switch
      ( long "with-ram"
          <> help
            ( "Take a full-machine snapshot: capture RAM + device model"
                ++ " + CPU state alongside the disk(s). This disk becomes"
                ++ " the vmstate carrier; every other writable qcow2 disk"
                ++ " on the same running VM gets a sibling block snapshot"
                ++ " under the same name. Requires the VM to be running"
                ++ " and QEMU >= 6.0. Rollback restores the running"
                ++ " machine state (no separate VmStart needed)."
            )
      )
  where
    parseQuiesceFlag :: String -> Either String QuiesceModeFlag
    parseQuiesceFlag s = case s of
      "auto" -> Right QuiesceFlagAuto
      "require" -> Right QuiesceFlagRequire
      "skip" -> Right QuiesceFlagSkip
      other -> Left ("expected auto|require|skip, got: " ++ other)

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
    <*> switch
      ( long "auto-stop"
          <> help
            ( "Orchestrate a graceful VM stop + rollback + start when"
                ++ " any attached VM is currently running/paused."
                ++ " Without this flag, the command refuses the rollback"
                ++ " on a running VM (QEMU has no online rollback)."
            )
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
        ( info
            snapshotCreateCommand
            ( progDesc
                "Create a snapshot (qcow2 only; live for running VMs, offline otherwise)"
            )
        )
        <> command
          "delete"
          (info snapshotDeleteCommand (progDesc "Delete a snapshot"))
        <> command
          "rollback"
          ( info
              snapshotRollbackCommand
              ( progDesc
                  "Rollback to a snapshot (VM must be stopped; pass --auto-stop to cycle)"
              )
          )
        <> command
          "merge"
          (info snapshotMergeCommand (progDesc "Merge a snapshot (VM must be stopped)"))
        <> command
          "list"
          (info snapshotListCommand (progDesc "List snapshots for a disk"))
    )
