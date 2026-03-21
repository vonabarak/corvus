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

    -- * Network interface commands
    whenNetIfAdd,
    whenNetIfRemove,
    whenNetIfList,

    -- * SSH key commands
    whenSshKeyCreate,
    whenSshKeyDelete,
    whenSshKeyList,
    whenSshKeyAttach,
    whenSshKeyDetach,
    whenSshKeyListForVm,

    -- * VM create/delete
    whenVmCreate,
    whenVmDelete,

    -- * Core commands
    whenPing,
    whenStatus,
    whenShutdown,

    -- * Low-level
    executeRequest,
    createTestServerState,
  )
where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Corvus.Client.Connection (Connection (..), ConnectionError)
import Corvus.Client.Rpc (DiskResult (..), NetIfResult (..), SharedDirResult (..), SnapshotResult (..), SshKeyResult (..), VmActionResult (..), VmCreateResult (..), VmDeleteResult (..))
import qualified Corvus.Client.Rpc as Rpc
import Corvus.Handlers (handleRequest)
import Corvus.Model (CacheType (..), DiskImage, DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, SharedDir, SharedDirCache, Snapshot, VmStatus)
import Corvus.Protocol (Request (..), Response (..), StatusInfo, VmDetails (..), VmInfo (..))
import Corvus.Qemu.Config (defaultQemuConfig)
import Corvus.Types (ServerState (..))
import Data.IORef (writeIORef)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql (SqlBackend)
import Test.DSL.Core (TestM, getDbPool, setLastResponse)
import qualified Test.Database as DB

--------------------------------------------------------------------------------
-- Direct Connection
--------------------------------------------------------------------------------

-- | Create a Connection that calls handleRequest directly
directConnection :: ServerState -> DB.TestEnv -> Connection
directConnection state env =
  Connection
    { connSendRequest = \req -> do
        resp <- handleRequest state req
        writeIORef (DB.teLastResponse env) (Just resp)
        pure (Right resp),
      connClose = pure ()
    }

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
  env <- asks id
  state <- liftIO $ createTestServerState pool
  let conn = directConnection state env
  resp <- liftIO $ handleRequest state req
  setLastResponse resp
  pure resp

-- | Execute an RPC call and return the result
executeRpc :: (Connection -> IO (Either ConnectionError a)) -> TestM a
executeRpc rpcCall = do
  pool <- getDbPool
  env <- asks id
  state <- liftIO $ createTestServerState pool
  let conn = directConnection state env
  result <- liftIO $ rpcCall conn
  case result of
    Left err -> error $ "Direct RPC failed: " <> show err
    Right a -> pure a

--------------------------------------------------------------------------------
-- VM Commands
--------------------------------------------------------------------------------

-- | List all VMs
vmList :: TestM [VmInfo]
vmList = executeRpc Rpc.listVms

-- | Show VM details
vmShow :: Int64 -> TestM (Maybe VmDetails)
vmShow vmId = executeRpc (\conn -> Rpc.showVm conn vmId)

-- | Start a VM
vmStart :: Int64 -> TestM VmActionResult
vmStart vmId = executeRpc (\conn -> Rpc.vmStart conn vmId)

-- | Stop a VM
vmStop :: Int64 -> TestM VmActionResult
vmStop vmId = executeRpc (\conn -> Rpc.vmStop conn vmId)

-- | Pause a VM
vmPause :: Int64 -> TestM VmActionResult
vmPause vmId = executeRpc (\conn -> Rpc.vmPause conn vmId)

-- | Reset a VM
vmReset :: Int64 -> TestM VmActionResult
vmReset vmId = executeRpc (\conn -> Rpc.vmReset conn vmId)

--------------------------------------------------------------------------------
-- Disk Commands
--------------------------------------------------------------------------------

-- | Create a disk image
diskCreate :: Text -> DriveFormat -> Int64 -> TestM DiskResult
diskCreate name format sizeMb =
  executeRpc (\conn -> Rpc.diskCreate conn name format sizeMb)

-- | Create an overlay disk image backed by an existing disk
diskCreateOverlay :: Text -> Int64 -> TestM DiskResult
diskCreateOverlay name baseDiskId =
  executeRpc (\conn -> Rpc.diskCreateOverlay conn name baseDiskId)

-- | Delete a disk image
diskDelete :: Int64 -> TestM DiskResult
diskDelete diskId = executeRpc (\conn -> Rpc.diskDelete conn diskId)

-- | Resize a disk image
diskResize :: Int64 -> Int64 -> TestM DiskResult
diskResize diskId newSizeMb =
  executeRpc (\conn -> Rpc.diskResize conn diskId newSizeMb)

-- | List all disk images
diskList :: TestM DiskResult
diskList = executeRpc Rpc.diskList

-- | Show disk image details
diskShow :: Int64 -> TestM DiskResult
diskShow diskId = executeRpc (\conn -> Rpc.diskShow conn diskId)

-- | Attach a disk to a VM
diskAttach :: Int64 -> Int64 -> DriveInterface -> Maybe DriveMedia -> TestM DiskResult
diskAttach vmId diskId interface media =
  executeRpc (\conn -> Rpc.diskAttach conn vmId diskId interface media False False CacheWriteback)

-- | Attach a disk to a VM in read-only mode
diskAttachReadOnly :: Int64 -> Int64 -> DriveInterface -> Maybe DriveMedia -> TestM DiskResult
diskAttachReadOnly vmId diskId interface media =
  executeRpc (\conn -> Rpc.diskAttach conn vmId diskId interface media True False CacheNone)

-- | Detach a drive from a VM
diskDetach :: Int64 -> Int64 -> TestM DiskResult
diskDetach vmId driveId = executeRpc (\conn -> Rpc.diskDetach conn vmId driveId)

--------------------------------------------------------------------------------
-- Snapshot Commands
--------------------------------------------------------------------------------

-- | Create a snapshot
snapshotCreate :: Int64 -> Text -> TestM SnapshotResult
snapshotCreate diskId name =
  executeRpc (\conn -> Rpc.snapshotCreate conn diskId name)

-- | Delete a snapshot
snapshotDelete :: Int64 -> Int64 -> TestM SnapshotResult
snapshotDelete diskId snapshotId =
  executeRpc (\conn -> Rpc.snapshotDelete conn diskId snapshotId)

-- | Rollback to a snapshot
snapshotRollback :: Int64 -> Int64 -> TestM SnapshotResult
snapshotRollback diskId snapshotId =
  executeRpc (\conn -> Rpc.snapshotRollback conn diskId snapshotId)

-- | Merge a snapshot
snapshotMerge :: Int64 -> Int64 -> TestM SnapshotResult
snapshotMerge diskId snapshotId =
  executeRpc (\conn -> Rpc.snapshotMerge conn diskId snapshotId)

-- | List snapshots for a disk
snapshotList :: Int64 -> TestM SnapshotResult
snapshotList diskId = executeRpc (\conn -> Rpc.snapshotList conn diskId)

--------------------------------------------------------------------------------
-- Shared Directory Commands
--------------------------------------------------------------------------------

-- | Add a shared directory to a VM
whenSharedDirAdd :: Int64 -> Text -> Text -> SharedDirCache -> Bool -> TestM SharedDirResult
whenSharedDirAdd vmId path tag cache readOnly =
  executeRpc (\conn -> Rpc.sharedDirAdd conn vmId path tag cache readOnly)

-- | Remove a shared directory from a VM
whenSharedDirRemove :: Int64 -> Int64 -> TestM SharedDirResult
whenSharedDirRemove vmId sharedDirId =
  executeRpc (\conn -> Rpc.sharedDirRemove conn vmId sharedDirId)

-- | List shared directories for a VM
whenSharedDirList :: Int64 -> TestM SharedDirResult
whenSharedDirList vmId = executeRpc (\conn -> Rpc.sharedDirList conn vmId)

--------------------------------------------------------------------------------
-- Network Interface Commands
--------------------------------------------------------------------------------

-- | Add a network interface to a VM
whenNetIfAdd :: Int64 -> NetInterfaceType -> Text -> Text -> TestM NetIfResult
whenNetIfAdd vmId ifaceType hostDevice mac =
  executeRpc (\conn -> Rpc.netIfAdd conn vmId ifaceType hostDevice mac)

-- | Remove a network interface from a VM
whenNetIfRemove :: Int64 -> Int64 -> TestM NetIfResult
whenNetIfRemove vmId netIfId =
  executeRpc (\conn -> Rpc.netIfRemove conn vmId netIfId)

-- | List network interfaces for a VM
whenNetIfList :: Int64 -> TestM NetIfResult
whenNetIfList vmId = executeRpc (\conn -> Rpc.netIfList conn vmId)

--------------------------------------------------------------------------------
-- SSH Key Commands
--------------------------------------------------------------------------------

-- | Create a new SSH key
whenSshKeyCreate :: Text -> Text -> TestM SshKeyResult
whenSshKeyCreate name publicKey =
  executeRpc (\conn -> Rpc.sshKeyCreate conn name publicKey)

-- | Delete an SSH key
whenSshKeyDelete :: Int64 -> TestM SshKeyResult
whenSshKeyDelete keyId = executeRpc (\conn -> Rpc.sshKeyDelete conn keyId)

-- | List all SSH keys
whenSshKeyList :: TestM SshKeyResult
whenSshKeyList = executeRpc Rpc.sshKeyList

-- | Attach an SSH key to a VM
whenSshKeyAttach :: Int64 -> Int64 -> TestM SshKeyResult
whenSshKeyAttach vmId keyId =
  executeRpc (\conn -> Rpc.sshKeyAttach conn vmId keyId)

-- | Detach an SSH key from a VM
whenSshKeyDetach :: Int64 -> Int64 -> TestM SshKeyResult
whenSshKeyDetach vmId keyId =
  executeRpc (\conn -> Rpc.sshKeyDetach conn vmId keyId)

-- | List SSH keys for a VM
whenSshKeyListForVm :: Int64 -> TestM SshKeyResult
whenSshKeyListForVm vmId = executeRpc (\conn -> Rpc.sshKeyListForVm conn vmId)

--------------------------------------------------------------------------------
-- VM Create/Delete Commands
--------------------------------------------------------------------------------

-- | Create a new VM
whenVmCreate :: Text -> Int -> Int -> Maybe Text -> TestM VmCreateResult
whenVmCreate name cpuCount ramMb description =
  executeRpc (\conn -> Rpc.vmCreate conn name cpuCount ramMb description)

-- | Delete a VM
whenVmDelete :: Int64 -> TestM VmDeleteResult
whenVmDelete vmId = executeRpc (\conn -> Rpc.vmDelete conn vmId)

--------------------------------------------------------------------------------
-- Core Commands
--------------------------------------------------------------------------------

-- | Send a ping request
whenPing :: TestM Response
whenPing = executeRequest ReqPing

-- | Get daemon status
whenStatus :: TestM Response
whenStatus = executeRequest ReqStatus

-- | Request daemon shutdown
whenShutdown :: TestM Response
whenShutdown = executeRequest ReqShutdown
