{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parsers for @crv disk@ subcommands.
module Corvus.Client.Parser.Disk
  ( diskCommandParser
  )
where

import Corvus.Client.Completion
import Corvus.Client.Parser.Utility
import Corvus.Client.Types
import qualified Data.Text as T
import Options.Applicative

-- | Parser for disk create
diskCreateCommand :: Parser Command
diskCreateCommand =
  DiskCreate
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the disk image"
      )
    <*> strOption
      ( long "format"
          <> short 'f'
          <> metavar "FORMAT"
          <> value "qcow2"
          <> help "Disk format: qcow2, raw, vmdk, vdi, vpc, vhdx (default: qcow2)"
          <> completeWith ["qcow2", "raw", "vmdk", "vdi", "vpc", "vhdx"]
      )
    <*> option
      parseSizeWithUnit
      ( long "size"
          <> short 's'
          <> metavar "SIZE"
          <> help "Disk size in MB (or with suffix: 10G, 100M)"
      )
    <*> optional
      ( strOption
          ( long "path"
              <> short 'p'
              <> metavar "PATH"
              <> help "Destination path. Trailing / = directory (filename auto-generated). Relative paths resolve against base images path."
          )
      )

-- | Parser for disk delete
diskDeleteCommand :: Parser Command
diskDeleteCommand =
  DiskDelete
    <$> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the disk image to delete"
          <> completer diskCompleter
      )

-- | Parser for disk resize
diskResizeCommand :: Parser Command
diskResizeCommand =
  DiskResize
    <$> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the disk image to resize"
          <> completer diskCompleter
      )
    <*> option
      parseSizeWithUnit
      ( long "size"
          <> short 's'
          <> metavar "SIZE"
          <> help "New size in MB (or with suffix: 20G, 2048M)"
      )

-- | Parser for disk list
diskListCommand :: Parser Command
diskListCommand = pure DiskList

-- | Parser for disk show
diskShowCommand :: Parser Command
diskShowCommand =
  DiskShow
    <$> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the disk image to show"
          <> completer diskCompleter
      )

-- | Parser for disk attach
diskAttachCommand :: Parser Command
diskAttachCommand =
  DiskAttach
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the disk image to attach"
          <> completer diskCompleter
      )
    <*> strOption
      ( long "interface"
          <> short 'i'
          <> metavar "INTERFACE"
          <> value "virtio"
          <> help "Drive interface: virtio, ide, scsi, sata, nvme (default: virtio)"
          <> completeWith ["virtio", "ide", "scsi", "sata", "nvme"]
      )
    <*> optional
      ( strOption
          ( long "media"
              <> short 'm'
              <> metavar "MEDIA"
              <> help "Media type: disk, cdrom"
              <> completeWith ["disk", "cdrom"]
          )
      )
    <*> switch
      ( long "read-only"
          <> help "Attach the disk in read-only mode (required for base images with overlays)"
      )
    <*> switch
      ( long "discard"
          <> help "Enable discard support (UNMAP/TRIM) (default: off)"
      )
    <*> strOption
      ( long "cache"
          <> metavar "CACHE_TYPE"
          <> value "writeback"
          <> help "Disk cache type: writeback, none, writethrough, directsync, unsafe (default: writeback)"
          <> completeWith ["writeback", "none", "writethrough", "directsync", "unsafe"]
      )

-- | Parser for disk detach
diskDetachCommand :: Parser Command
diskDetachCommand =
  DiskDetach
    <$> argument
      (T.pack <$> str)
      ( metavar "VM"
          <> help "Name or ID of the VM"
          <> completer vmCompleter
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the disk image to detach"
          <> completer diskCompleter
      )

-- | Parser for disk register (existing local file, no copy)
diskRegisterCommand :: Parser Command
diskRegisterCommand =
  DiskRegisterCmd
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the disk image"
      )
    <*> argument
      str
      ( metavar "PATH"
          <> help "Path to existing disk image file (local only, not copied)"
      )
    <*> optional
      ( strOption
          ( long "format"
              <> short 'f'
              <> metavar "FORMAT"
              <> help "Disk format (auto-detected if not specified): qcow2, raw, vmdk, vdi, vpc, vhdx"
              <> completeWith ["qcow2", "raw", "vmdk", "vdi", "vpc", "vhdx"]
          )
      )
    <*> optional
      ( T.pack
          <$> strOption
            ( long "backing"
                <> short 'b'
                <> metavar "BACKING"
                <> help "Name or ID of the backing disk image (use when registering an overlay)"
                <> completer diskCompleter
            )
      )

-- | Parser for disk import (copies local file or downloads URL)
diskImportCommand :: Parser Command
diskImportCommand =
  DiskImport
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the imported disk image"
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "SOURCE"
          <> help "Source file path or HTTP/HTTPS URL"
      )
    <*> optional
      ( T.pack
          <$> strOption
            ( long "path"
                <> short 'p'
                <> metavar "PATH"
                <> help "Destination path (trailing / = directory, relative = under base images dir)"
            )
      )
    <*> optional
      ( T.pack
          <$> strOption
            ( long "format"
                <> short 'f'
                <> metavar "FORMAT"
                <> help "Disk format (auto-detected if not specified): qcow2, raw, vmdk, vdi, vpc, vhdx"
                <> completeWith ["qcow2", "raw", "vmdk", "vdi", "vpc", "vhdx"]
            )
      )
    <*> waitOptionsParser

-- | Parser for disk overlay
diskOverlayCommand :: Parser Command
diskOverlayCommand =
  DiskCreateOverlay
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the overlay disk image"
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the base disk image to overlay"
          <> completer diskCompleter
      )
    <*> optional
      ( strOption
          ( long "path"
              <> short 'p'
              <> metavar "PATH"
              <> help "Destination path. Trailing / = directory (filename auto-generated). Relative paths resolve against base images path."
          )
      )

-- | Parser for disk clone
diskCloneCommand :: Parser Command
diskCloneCommand =
  DiskClone
    <$> argument
      (T.pack <$> str)
      ( metavar "NAME"
          <> help "Name for the cloned disk image"
      )
    <*> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the base disk image to clone"
          <> completer diskCompleter
      )
    <*> optional
      ( strOption
          ( long "path"
              <> short 'p'
              <> metavar "PATH"
              <> help "Destination path. Trailing / = directory (filename auto-generated). Relative paths resolve against base images path."
          )
      )

-- | Parser for disk rebase
diskRebaseCommand :: Parser Command
diskRebaseCommand =
  DiskRebase
    <$> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the overlay disk image to rebase"
          <> completer diskCompleter
      )
    <*> optional
      ( strOption
          ( long "backing"
              <> short 'b'
              <> metavar "BACKING"
              <> help "New backing image (name or ID). Omit to flatten (merge backing into overlay)."
              <> completer diskCompleter
          )
      )
    <*> switch
      ( long "unsafe"
          <> help "Only update backing pointer without data transformation (use when old and new backing have identical content)"
      )

-- | Parser for disk refresh
diskRefreshCommand :: Parser Command
diskRefreshCommand =
  DiskRefresh
    <$> argument
      (T.pack <$> str)
      ( metavar "DISK"
          <> help "Name or ID of the disk to refresh"
          <> completer diskCompleter
      )

-- | Parser for all disk subcommands
diskCommandParser :: Parser Command
diskCommandParser =
  subparser
    ( command
        "create"
        (info diskCreateCommand (progDesc "Create a new disk image"))
        <> command
          "overlay"
          (info diskOverlayCommand (progDesc "Create a qcow2 overlay backed by an existing disk"))
        <> command
          "register"
          (info diskRegisterCommand (progDesc "Register an existing local disk image file (no copy)"))
        <> command
          "import"
          (info diskImportCommand (progDesc "Import a disk image from a local file or URL (copies to destination)"))
        <> command
          "delete"
          (info diskDeleteCommand (progDesc "Delete a disk image"))
        <> command
          "resize"
          (info diskResizeCommand (progDesc "Resize a disk image"))
        <> command
          "list"
          (info diskListCommand (progDesc "List all disk images"))
        <> command
          "show"
          (info diskShowCommand (progDesc "Show disk image details"))
        <> command
          "clone"
          (info diskCloneCommand (progDesc "Clone a disk image"))
        <> command
          "rebase"
          (info diskRebaseCommand (progDesc "Rebase overlay to a different backing image, or flatten"))
        <> command
          "attach"
          (info diskAttachCommand (progDesc "Attach a disk to a VM"))
        <> command
          "detach"
          (info diskDetachCommand (progDesc "Detach a disk from a VM"))
        <> command
          "refresh"
          (info diskRefreshCommand (progDesc "Refresh disk image size from qemu-img"))
    )
