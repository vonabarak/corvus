-- | DSL primitives for command execution (When phase).
-- Provides functions to invoke handlers and capture responses.
module Test.DSL.When
  ( -- * VM commands
    vmList
  , vmShow
  , vmStart
  , vmStop
  , vmPause
  , vmReset

    -- * Disk commands
  , diskCreate
  , diskCreateOverlay
  , diskRegister
  , diskClone
  , diskDelete
  , diskResize
  , diskList
  , diskShow
  , diskAttach
  , diskAttachReadOnly
  , diskDetach

    -- * Snapshot commands
  , snapshotCreate
  , snapshotDelete
  , snapshotRollback
  , snapshotMerge
  , snapshotList

    -- * Shared directory commands
  , whenSharedDirAdd
  , whenSharedDirRemove
  , whenSharedDirList

    -- * Network interface commands
  , whenNetIfAdd
  , whenNetIfRemove
  , whenNetIfList

    -- * SSH key commands
  , whenSshKeyCreate
  , whenSshKeyDelete
  , whenSshKeyList
  , whenSshKeyAttach
  , whenSshKeyDetach
  , whenSshKeyListForVm

    -- * VM edit
  , whenVmEdit

    -- * VM create/delete
  , whenVmCreate
  , whenVmDelete

    -- * Core commands
  , whenPing
  , whenStatus
  , whenShutdown

    -- * Apply
  , whenApply

    -- * Network commands
  , whenNetworkCreate
  , whenNetworkDelete
  , whenNetworkList
  , whenNetworkShow

    -- * Template commands
  , whenTemplateCreate
  , whenTemplateDelete
  , whenTemplateList
  , whenTemplateShow

    -- * Guest exec commands
  , whenGuestExec

    -- * Low-level
  , executeRequest
  , createTestServerState
  )
where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Corvus.Client.Connection (Connection (..), ConnectionError)
import Corvus.Client.Rpc (DiskResult (..), GuestExecResult (..), NetIfResult (..), NetworkResult (..), SharedDirResult (..), SnapshotResult (..), SshKeyResult (..), TemplateResult (..), VmActionResult (..), VmCreateResult (..), VmDeleteResult (..), VmEditResult (..))
import qualified Corvus.Client.Rpc as Rpc
import Corvus.Handlers (handleRequest)
import Corvus.Model (CacheType (..), DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, SharedDirCache)
import Corvus.Protocol (Request (..), Response (..), VmDetails (..), VmInfo (..))
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Corvus.Types (ServerState (..))
import Data.IORef (writeIORef)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql (SqlBackend)
import Test.DSL.Core (TestM, getDbPool, getTempDir, setLastResponse)
import qualified Test.Database as DB
import Test.Settings (getTestLogLevel)

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
        pure (Right resp)
    , connClose = pure ()
    }

--------------------------------------------------------------------------------
-- Server State
--------------------------------------------------------------------------------

-- | Create a ServerState for testing with the given database pool and base path
createTestServerState :: Pool SqlBackend -> FilePath -> IO ServerState
createTestServerState pool basePath = do
  startTime <- getCurrentTime
  connCount <- newTVarIO 0
  shutdownFlag <- newTVarIO False
  namespacePid <- newTVarIO Nothing
  logLevel <- getTestLogLevel
  pure
    ServerState
      { ssStartTime = startTime
      , ssConnectionCount = connCount
      , ssShutdownFlag = shutdownFlag
      , ssDbPool = pool
      , ssQemuConfig = defaultQemuConfig {qcBasePath = Just basePath}
      , ssLogLevel = logLevel
      , ssNamespacePid = namespacePid
      }

--------------------------------------------------------------------------------
-- Low-level Request Execution
--------------------------------------------------------------------------------

-- | Execute a request and return the response
executeRequest :: Request -> TestM Response
executeRequest req = do
  pool <- getDbPool
  tempDir <- getTempDir
  env <- asks id
  state <- liftIO $ createTestServerState pool tempDir
  let conn = directConnection state env
  resp <- liftIO $ handleRequest state req
  setLastResponse resp
  pure resp

-- | Execute an RPC call and return the result
executeRpc :: (Connection -> IO (Either ConnectionError a)) -> TestM a
executeRpc rpcCall = do
  pool <- getDbPool
  tempDir <- getTempDir
  env <- asks id
  state <- liftIO $ createTestServerState pool tempDir
  let conn = directConnection state env
  result <- liftIO $ rpcCall conn
  case result of
    Left err -> error $ "Direct RPC failed: " <> show err
    Right a -> pure a

-- | Convert an Int64 database ID to a Text ref for RPC calls
toRef :: Int64 -> Text
toRef = T.pack . show

--------------------------------------------------------------------------------
-- VM Commands
--------------------------------------------------------------------------------

-- | List all VMs
vmList :: TestM [VmInfo]
vmList = executeRpc Rpc.listVms

-- | Show VM details
vmShow :: Int64 -> TestM (Maybe VmDetails)
vmShow vmId = executeRpc (`Rpc.showVm` toRef vmId)

-- | Start a VM (no wait)
vmStart :: Int64 -> TestM VmActionResult
vmStart vmId = executeRpc (\conn -> Rpc.vmStart conn (toRef vmId) False)

-- | Stop a VM (no wait)
vmStop :: Int64 -> TestM VmActionResult
vmStop vmId = executeRpc (\conn -> Rpc.vmStop conn (toRef vmId) False)

-- | Pause a VM
vmPause :: Int64 -> TestM VmActionResult
vmPause vmId = executeRpc (`Rpc.vmPause` toRef vmId)

-- | Reset a VM
vmReset :: Int64 -> TestM VmActionResult
vmReset vmId = executeRpc (`Rpc.vmReset` toRef vmId)

--------------------------------------------------------------------------------
-- Disk Commands
--------------------------------------------------------------------------------

-- | Create a disk image
diskCreate :: Text -> DriveFormat -> Int64 -> TestM DiskResult
diskCreate name format sizeMb =
  executeRpc (\conn -> Rpc.diskCreate conn name format sizeMb Nothing)

-- | Create an overlay disk image backed by an existing disk
diskCreateOverlay :: Text -> Int64 -> Maybe Text -> TestM DiskResult
diskCreateOverlay name baseDiskId optDirPath =
  executeRpc (\conn -> Rpc.diskCreateOverlay conn name (toRef baseDiskId) optDirPath)

-- | Register an existing disk image file
diskRegister :: Text -> Text -> DriveFormat -> TestM DiskResult
diskRegister name filePath format =
  executeRpc (\conn -> Rpc.diskRegister conn name filePath (Just format))

-- | Clone a disk image
diskClone :: Text -> Int64 -> Maybe Text -> TestM DiskResult
diskClone name baseDiskId optionalPath =
  executeRpc (\conn -> Rpc.diskClone conn name (toRef baseDiskId) optionalPath)

-- | Delete a disk image
diskDelete :: Int64 -> TestM DiskResult
diskDelete diskId = executeRpc (`Rpc.diskDelete` toRef diskId)

-- | Resize a disk image
diskResize :: Int64 -> Int64 -> TestM DiskResult
diskResize diskId newSizeMb =
  executeRpc (\conn -> Rpc.diskResize conn (toRef diskId) newSizeMb)

-- | List all disk images
diskList :: TestM DiskResult
diskList = executeRpc Rpc.diskList

-- | Show disk image details
diskShow :: Int64 -> TestM DiskResult
diskShow diskId = executeRpc (`Rpc.diskShow` toRef diskId)

-- | Attach a disk to a VM
diskAttach :: Int64 -> Int64 -> DriveInterface -> Maybe DriveMedia -> TestM DiskResult
diskAttach vmId diskId interface media =
  executeRpc (\conn -> Rpc.diskAttach conn (toRef vmId) (toRef diskId) interface media False False CacheWriteback)

-- | Attach a disk to a VM in read-only mode
diskAttachReadOnly :: Int64 -> Int64 -> DriveInterface -> Maybe DriveMedia -> TestM DiskResult
diskAttachReadOnly vmId diskId interface media =
  executeRpc (\conn -> Rpc.diskAttach conn (toRef vmId) (toRef diskId) interface media True False CacheNone)

-- | Detach a drive from a VM
diskDetach :: Int64 -> Int64 -> TestM DiskResult
diskDetach vmId driveId = executeRpc (\conn -> Rpc.diskDetach conn (toRef vmId) (toRef driveId))

--------------------------------------------------------------------------------
-- Snapshot Commands
--------------------------------------------------------------------------------

-- | Create a snapshot
snapshotCreate :: Int64 -> Text -> TestM SnapshotResult
snapshotCreate diskId name =
  executeRpc (\conn -> Rpc.snapshotCreate conn (toRef diskId) name)

-- | Delete a snapshot
snapshotDelete :: Int64 -> Int64 -> TestM SnapshotResult
snapshotDelete diskId snapshotId =
  executeRpc (\conn -> Rpc.snapshotDelete conn (toRef diskId) (toRef snapshotId))

-- | Rollback to a snapshot
snapshotRollback :: Int64 -> Int64 -> TestM SnapshotResult
snapshotRollback diskId snapshotId =
  executeRpc (\conn -> Rpc.snapshotRollback conn (toRef diskId) (toRef snapshotId))

-- | Merge a snapshot
snapshotMerge :: Int64 -> Int64 -> TestM SnapshotResult
snapshotMerge diskId snapshotId =
  executeRpc (\conn -> Rpc.snapshotMerge conn (toRef diskId) (toRef snapshotId))

-- | List snapshots for a disk
snapshotList :: Int64 -> TestM SnapshotResult
snapshotList diskId = executeRpc (`Rpc.snapshotList` toRef diskId)

--------------------------------------------------------------------------------
-- Shared Directory Commands
--------------------------------------------------------------------------------

-- | Add a shared directory to a VM
whenSharedDirAdd :: Int64 -> Text -> Text -> SharedDirCache -> Bool -> TestM SharedDirResult
whenSharedDirAdd vmId path tag cache readOnly =
  executeRpc (\conn -> Rpc.sharedDirAdd conn (toRef vmId) path tag cache readOnly)

-- | Remove a shared directory from a VM
whenSharedDirRemove :: Int64 -> Int64 -> TestM SharedDirResult
whenSharedDirRemove vmId sharedDirId =
  executeRpc (\conn -> Rpc.sharedDirRemove conn (toRef vmId) (toRef sharedDirId))

-- | List shared directories for a VM
whenSharedDirList :: Int64 -> TestM SharedDirResult
whenSharedDirList vmId = executeRpc (`Rpc.sharedDirList` toRef vmId)

--------------------------------------------------------------------------------
-- Network Interface Commands
--------------------------------------------------------------------------------

-- | Add a network interface to a VM
whenNetIfAdd :: Int64 -> NetInterfaceType -> Text -> Maybe Text -> TestM NetIfResult
whenNetIfAdd vmId ifaceType hostDevice mac =
  executeRpc (\conn -> Rpc.netIfAdd conn (toRef vmId) ifaceType hostDevice mac Nothing)

-- | Remove a network interface from a VM
whenNetIfRemove :: Int64 -> Int64 -> TestM NetIfResult
whenNetIfRemove vmId netIfId =
  executeRpc (\conn -> Rpc.netIfRemove conn (toRef vmId) netIfId)

-- | List network interfaces for a VM
whenNetIfList :: Int64 -> TestM NetIfResult
whenNetIfList vmId = executeRpc (`Rpc.netIfList` toRef vmId)

--------------------------------------------------------------------------------
-- SSH Key Commands
--------------------------------------------------------------------------------

-- | Create a new SSH key
whenSshKeyCreate :: Text -> Text -> TestM SshKeyResult
whenSshKeyCreate name publicKey =
  executeRpc (\conn -> Rpc.sshKeyCreate conn name publicKey)

-- | Delete an SSH key
whenSshKeyDelete :: Int64 -> TestM SshKeyResult
whenSshKeyDelete keyId = executeRpc (`Rpc.sshKeyDelete` toRef keyId)

-- | List all SSH keys
whenSshKeyList :: TestM SshKeyResult
whenSshKeyList = executeRpc Rpc.sshKeyList

-- | Attach an SSH key to a VM
whenSshKeyAttach :: Int64 -> Int64 -> TestM SshKeyResult
whenSshKeyAttach vmId keyId =
  executeRpc (\conn -> Rpc.sshKeyAttach conn (toRef vmId) (toRef keyId))

-- | Detach an SSH key from a VM
whenSshKeyDetach :: Int64 -> Int64 -> TestM SshKeyResult
whenSshKeyDetach vmId keyId =
  executeRpc (\conn -> Rpc.sshKeyDetach conn (toRef vmId) (toRef keyId))

-- | List SSH keys for a VM
whenSshKeyListForVm :: Int64 -> TestM SshKeyResult
whenSshKeyListForVm vmId = executeRpc (`Rpc.sshKeyListForVm` toRef vmId)

--------------------------------------------------------------------------------
-- VM Edit Commands
--------------------------------------------------------------------------------

-- | Edit VM properties
whenVmEdit :: Int64 -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> TestM VmEditResult
whenVmEdit vmId mCpus mRam mDesc mHeadless =
  executeRpc (\conn -> Rpc.vmEdit conn (toRef vmId) mCpus mRam mDesc mHeadless Nothing Nothing)

--------------------------------------------------------------------------------
-- VM Create/Delete Commands
--------------------------------------------------------------------------------

-- | Create a new VM
whenVmCreate :: Text -> Int -> Int -> Maybe Text -> TestM VmCreateResult
whenVmCreate name cpuCount ramMb description =
  executeRpc (\conn -> Rpc.vmCreate conn name cpuCount ramMb description False False False)

-- | Delete a VM
whenVmDelete :: Int64 -> TestM VmDeleteResult
whenVmDelete vmId = executeRpc (`Rpc.vmDelete` toRef vmId)

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

--------------------------------------------------------------------------------
-- Apply Commands
--------------------------------------------------------------------------------

-- | Apply environment from YAML content
whenApply :: Text -> TestM Response
whenApply yaml = executeRequest (ReqApply yaml False)

--------------------------------------------------------------------------------
-- Network Commands
--------------------------------------------------------------------------------

-- | Create a network
whenNetworkCreate :: Text -> Text -> TestM NetworkResult
whenNetworkCreate name subnet =
  executeRpc (\conn -> Rpc.networkCreate conn name subnet False)

-- | Create a network with DHCP enabled
whenNetworkCreateWithDhcp :: Text -> Text -> TestM NetworkResult
whenNetworkCreateWithDhcp name subnet =
  executeRpc (\conn -> Rpc.networkCreate conn name subnet True)

-- | Delete a network
whenNetworkDelete :: Int64 -> TestM NetworkResult
whenNetworkDelete nwId =
  executeRpc (\conn -> Rpc.networkDelete conn (toRef nwId))

-- | List all networks
whenNetworkList :: TestM NetworkResult
whenNetworkList = executeRpc Rpc.networkList

-- | Show network details
whenNetworkShow :: Int64 -> TestM NetworkResult
whenNetworkShow nwId =
  executeRpc (\conn -> Rpc.networkShow conn (toRef nwId))

--------------------------------------------------------------------------------
-- Template Commands
--------------------------------------------------------------------------------

-- | Create a template from YAML
whenTemplateCreate :: Text -> TestM TemplateResult
whenTemplateCreate tplYaml =
  executeRpc (`Rpc.templateCreate` tplYaml)

-- | Delete a template
whenTemplateDelete :: Int64 -> TestM TemplateResult
whenTemplateDelete tplId =
  executeRpc (\conn -> Rpc.templateDelete conn (toRef tplId))

-- | List all templates
whenTemplateList :: TestM TemplateResult
whenTemplateList = executeRpc Rpc.templateList

-- | Show template details
whenTemplateShow :: Int64 -> TestM TemplateResult
whenTemplateShow tplId =
  executeRpc (\conn -> Rpc.templateShow conn (toRef tplId))

--------------------------------------------------------------------------------
-- Guest Exec Commands
--------------------------------------------------------------------------------

-- | Execute a command in a VM via guest agent
whenGuestExec :: Int64 -> Text -> TestM GuestExecResult
whenGuestExec vmId cmd =
  executeRpc (\conn -> Rpc.vmExec conn (toRef vmId) cmd)
