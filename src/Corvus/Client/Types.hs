-- | Types for the Corvus client CLI.
module Corvus.Client.Types
  ( -- * Command line options
    Options (..),
    Command (..),
  )
where

import Data.Int (Int64)
import Data.Text (Text)

-- | Command line options
data Options = Options
  { optSocket :: Maybe FilePath,
    optTcp :: Bool,
    optHost :: String,
    optPort :: Int,
    optCommand :: Command
  }
  deriving (Show)

-- | Available commands
data Command
  = Ping
  | Status
  | Shutdown
  | -- VM commands
    VmList
  | VmShow !Int64
  | VmStart !Int64
  | VmStop !Int64
  | VmPause !Int64
  | VmReset !Int64
  | -- | View VM via SPICE (runs remote-viewer)
    VmView !Int64
  | -- | Connect to VM's HMP monitor
    VmMonitor !Int64
  | -- Disk image commands

    -- | Create disk image (name, format, sizeMb)
    DiskCreate !Text !Text !Int64
  | -- | Delete disk image
    DiskDelete !Int64
  | -- | Resize disk image (diskId, newSizeMb)
    DiskResize !Int64 !Int64
  | -- | List all disk images
    DiskList
  | -- | Show disk image details
    DiskShow !Int64
  | -- | Attach disk to VM (vmId, diskId, interface, media)
    DiskAttach !Int64 !Int64 !Text !(Maybe Text)
  | -- | Detach disk from VM (vmId, driveId)
    DiskDetach !Int64 !Int64
  | -- Snapshot commands

    -- | Create snapshot (diskId, name)
    SnapshotCreate !Int64 !Text
  | -- | Delete snapshot (diskId, snapshotId)
    SnapshotDelete !Int64 !Int64
  | -- | Rollback to snapshot (diskId, snapshotId)
    SnapshotRollback !Int64 !Int64
  | -- | Merge snapshot (diskId, snapshotId)
    SnapshotMerge !Int64 !Int64
  | -- | List snapshots (diskId)
    SnapshotList !Int64
  deriving (Show)
