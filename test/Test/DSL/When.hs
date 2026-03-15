-- | DSL primitives for command execution (When phase).
-- Provides functions to invoke handlers and capture responses.
module Test.DSL.When
  ( -- * VM commands
    vmList,
    vmShow,
    vmStart,
    vmStop,
    vmPause,
    vmReset,

    -- * Disk commands
    diskCreate,
    diskCreateOverlay,
    diskDelete,
    diskResize,
    diskList,
    diskShow,
    diskAttach,
    diskAttachReadOnly,
    diskDetach,

    -- * Snapshot commands
    snapshotCreate,
    snapshotDelete,
    snapshotRollback,
    snapshotMerge,
    snapshotList,

    -- * Shared directory commands
    whenSharedDirAdd,
    whenSharedDirRemove,
    whenSharedDirList,

    -- * Low-level
    executeRequest,
    createTestServerState,
  )
where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Corvus.Handlers (handleRequest)
import Corvus.Model (CacheType (..), DriveFormat, DriveInterface, DriveMedia, SharedDirCache)
import Corvus.Protocol
import Corvus.Qemu.Config (defaultQemuConfig)
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql (SqlBackend)
import Test.DSL.Core (TestM, getDbPool, setLastResponse)

--------------------------------------------------------------------------------
-- Server State
--------------------------------------------------------------------------------

-- | Create a ServerState for testing with the given database pool
createTestServerState :: Pool SqlBackend -> IO ServerState
createTestServerState pool = do
  startTime <- getCurrentTime
  connCount <- newTVarIO 0
  shutdownFlag <- newTVarIO False
  pure
    ServerState
      { ssStartTime = startTime,
        ssConnectionCount = connCount,
        ssShutdownFlag = shutdownFlag,
        ssDbPool = pool,
        ssQemuConfig = defaultQemuConfig
      }

--------------------------------------------------------------------------------
-- Low-level Request Execution
--------------------------------------------------------------------------------

-- | Execute a request and return the response
executeRequest :: Request -> TestM Response
executeRequest req = do
  pool <- getDbPool
  state <- liftIO $ createTestServerState pool
  resp <- liftIO $ handleRequest state req
  setLastResponse resp
  pure resp

--------------------------------------------------------------------------------
-- VM Commands
--------------------------------------------------------------------------------

-- | List all VMs
vmList :: TestM Response
vmList = executeRequest ReqListVms

-- | Show VM details
vmShow :: Int64 -> TestM Response
vmShow vmId = executeRequest (ReqShowVm vmId)

-- | Start a VM
vmStart :: Int64 -> TestM Response
vmStart vmId = executeRequest (ReqVmStart vmId)

-- | Stop a VM
vmStop :: Int64 -> TestM Response
vmStop vmId = executeRequest (ReqVmStop vmId)

-- | Pause a VM
vmPause :: Int64 -> TestM Response
vmPause vmId = executeRequest (ReqVmPause vmId)

-- | Reset a VM
vmReset :: Int64 -> TestM Response
vmReset vmId = executeRequest (ReqVmReset vmId)

--------------------------------------------------------------------------------
-- Disk Commands
--------------------------------------------------------------------------------

-- | Create a disk image
diskCreate :: Text -> DriveFormat -> Int64 -> TestM Response
diskCreate name format sizeMb =
  executeRequest (ReqDiskCreate name format sizeMb)

-- | Create an overlay disk image backed by an existing disk
diskCreateOverlay :: Text -> Int64 -> TestM Response
diskCreateOverlay name baseDiskId =
  executeRequest (ReqDiskCreateOverlay name baseDiskId)

-- | Delete a disk image
diskDelete :: Int64 -> TestM Response
diskDelete diskId = executeRequest (ReqDiskDelete diskId)

-- | Resize a disk image
diskResize :: Int64 -> Int64 -> TestM Response
diskResize diskId newSizeMb =
  executeRequest (ReqDiskResize diskId newSizeMb)

-- | List all disk images
diskList :: TestM Response
diskList = executeRequest ReqDiskList

-- | Show disk image details
diskShow :: Int64 -> TestM Response
diskShow diskId = executeRequest (ReqDiskShow diskId)

-- | Attach a disk to a VM
diskAttach :: Int64 -> Int64 -> DriveInterface -> Maybe DriveMedia -> TestM Response
diskAttach vmId diskId interface media =
  executeRequest (ReqDiskAttach vmId diskId interface media False False CacheWriteback)

-- | Attach a disk to a VM in read-only mode
diskAttachReadOnly :: Int64 -> Int64 -> DriveInterface -> Maybe DriveMedia -> TestM Response
diskAttachReadOnly vmId diskId interface media =
  executeRequest (ReqDiskAttach vmId diskId interface media True False CacheNone)

-- | Detach a drive from a VM
diskDetach :: Int64 -> Int64 -> TestM Response
diskDetach vmId driveId = executeRequest (ReqDiskDetach vmId driveId)

--------------------------------------------------------------------------------
-- Snapshot Commands
--------------------------------------------------------------------------------

-- | Create a snapshot
snapshotCreate :: Int64 -> Text -> TestM Response
snapshotCreate diskId name =
  executeRequest (ReqSnapshotCreate diskId name)

-- | Delete a snapshot
snapshotDelete :: Int64 -> Int64 -> TestM Response
snapshotDelete diskId snapshotId =
  executeRequest (ReqSnapshotDelete diskId snapshotId)

-- | Rollback to a snapshot
snapshotRollback :: Int64 -> Int64 -> TestM Response
snapshotRollback diskId snapshotId =
  executeRequest (ReqSnapshotRollback diskId snapshotId)

-- | Merge a snapshot
snapshotMerge :: Int64 -> Int64 -> TestM Response
snapshotMerge diskId snapshotId =
  executeRequest (ReqSnapshotMerge diskId snapshotId)

-- | List snapshots for a disk
snapshotList :: Int64 -> TestM Response
snapshotList diskId = executeRequest (ReqSnapshotList diskId)

--------------------------------------------------------------------------------
-- Shared Directory Commands
--------------------------------------------------------------------------------

-- | Add a shared directory to a VM
whenSharedDirAdd :: Int64 -> Text -> Text -> SharedDirCache -> Bool -> TestM Response
whenSharedDirAdd vmId path tag cache readOnly =
  executeRequest (ReqSharedDirAdd vmId path tag cache readOnly)

-- | Remove a shared directory from a VM
whenSharedDirRemove :: Int64 -> Int64 -> TestM Response
whenSharedDirRemove vmId sharedDirId =
  executeRequest (ReqSharedDirRemove vmId sharedDirId)

-- | List shared directories for a VM
whenSharedDirList :: Int64 -> TestM Response
whenSharedDirList vmId = executeRequest (ReqSharedDirList vmId)
