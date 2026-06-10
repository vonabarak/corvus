{-# LANGUAGE OverloadedStrings #-}

-- | DSL primitives for command execution (When phase).
-- Invokes the daemon's 'Action' types directly against a freshly
-- created 'ServerState'. This bypasses the Cap'n Proto wire entirely
-- — the test DSL exercises the business logic; end-to-end wire
-- coverage lives in 'Corvus.CapnpServerSpec'.
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
  , diskRegisterWithBacking
  , diskClone
  , diskImport
  , diskRebase
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

    -- * Node commands
  , whenNodeEditNetdDisabled

    -- * Template commands
  , whenTemplateCreate
  , whenTemplateUpdate
  , whenTemplateDelete
  , whenTemplateList
  , whenTemplateShow
  , whenTemplateInstantiate

    -- * Guest exec commands
  , whenGuestExec

    -- * Cloud-init config commands
  , whenCloudInitSet
  , whenCloudInitGet
  , whenCloudInitDelete

    -- * Low-level
  , createTestServerState
  , withState
  )
where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Corvus.Action (runAction, runActionAsync)
import Corvus.Handlers
import Corvus.Handlers.Apply (ApplyAction (..), handleApplyValidate)
import Corvus.Handlers.Build ()
import Corvus.Handlers.CloudInit (CloudInitDelete (..), CloudInitSet (..), handleCloudInitGet)
import Corvus.Handlers.Disk
  ( DiskClone (..)
  , DiskCreate (..)
  , DiskCreateOverlay (..)
  , DiskDelete (..)
  , DiskRefresh (..)
  , DiskRegister (..)
  , DiskResize (..)
  , handleDiskList
  , handleDiskShow
  )
import Corvus.Handlers.Disk.Attach (DiskAttach (..), DiskDetachByDisk (..))
import Corvus.Handlers.Disk.Import (DiskImportAction (..))
import Corvus.Handlers.Disk.Rebase (DiskRebase (..))
import Corvus.Handlers.Disk.Snapshot (SnapshotCreate (..), SnapshotDelete (..), SnapshotMerge (..), SnapshotRollback (..), handleSnapshotList)
import Corvus.Handlers.GuestExec (GuestExec (..))
import Corvus.Handlers.NetIf (NetIfAdd (..), NetIfRemove (..), handleNetIfList)
import Corvus.Handlers.Network (NetworkCreate (..), NetworkDelete (..), handleNetworkList, handleNetworkShow)
import Corvus.Handlers.Node (NodeEdit (..))
import Corvus.Handlers.Resolve
import Corvus.Handlers.SharedDir (SharedDirAdd (..), SharedDirRemove (..), handleSharedDirList)
import Corvus.Handlers.SshKey (SshKeyAttach (..), SshKeyCreate (..), SshKeyDelete (..), SshKeyDetach (..), handleSshKeyList, handleSshKeyListForVm)
import Corvus.Handlers.Template (TemplateCreate (..), TemplateDelete (..), TemplateInstantiate (..), TemplateUpdate (..), handleTemplateList, handleTemplateShow)
import Corvus.Handlers.Vm (VmDelete (..), VmEdit (..), VmPause (..), VmReset (..), VmStart (..), VmStop (..), handleVmList, handleVmShow)
import qualified Corvus.Handlers.Vm as VmHandlers
import Corvus.Model (CacheType (..), DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, SharedDirCache)
import qualified Corvus.Model as M
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Protocol (Ref (..), Response (..), VmDetails (..), VmInfo (..))
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Corvus.Types (NodeConns (..), ServerState (..), newAutostartFlags, registerNodeConns)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (toSqlKey)
import Test.DSL.Core (TestM, getDbPool, getTempDir, setLastResponse)
import qualified Test.Database as DB
import Test.Settings (getTestLogLevel)

--------------------------------------------------------------------------------
-- Server State
--------------------------------------------------------------------------------

-- | Create a ServerState for testing with the given database pool and base path
createTestServerState :: Pool SqlBackend -> FilePath -> IO ServerState
createTestServerState pool basePath = do
  startTime <- getCurrentTime
  connCount <- newTVarIO 0
  shutdownFlag <- newTVarIO False
  agents <- newTVarIO mempty
  gaSubs <- newTVarIO mempty
  taskSubs <- newTVarIO mempty
  vmStatsRing <- newTVarIO mempty
  vmStatsSubs <- newTVarIO mempty
  vsockLocks <- newTVarIO mempty
  spiceLock <- newMVar ()
  reservedRam <- newTVarIO mempty
  taskCancels <- newTVarIO mempty
  taskThreads <- newTVarIO mempty
  logLevel <- getTestLogLevel
  let state =
        ServerState
          { ssStartTime = startTime
          , ssConnectionCount = connCount
          , ssShutdownFlag = shutdownFlag
          , ssDbPool = pool
          , ssQemuConfig = defaultQemuConfig {qcBasePath = Just basePath}
          , ssLogLevel = logLevel
          , ssAgents = agents
          , ssGuestAgentSubs = gaSubs
          , ssTaskProgressSubs = taskSubs
          , ssVmStatsRing = vmStatsRing
          , ssVmStatsSubs = vmStatsSubs
          , ssVsockCidLocks = vsockLocks
          , ssSpicePortLock = spiceLock
          , ssTaskCancels = taskCancels
          , ssTaskThreads = taskThreads
          , ssReservedRam = reservedRam
          , ssTlsConfig = Nothing
          }
  -- Register a stub 'NodeConns' under nodeId=1 (the bootstrap
  -- 'test-node' row 'insertDefaultTestNode' installs). Both agent
  -- caps stay 'Nothing' — handlers that hit the agent path land
  -- in the "nodeagent unavailable for node 1" branch, but
  -- handlers that only need a node to *route through* (via
  -- 'singleRegisteredNode') succeed past the registry check.
  -- 'ncSupervisor' is a no-op async because no real reconnect
  -- loop runs in unit tests.
  stubSup <- async (pure ())
  (vmAS, netAS) <- newAutostartFlags
  registerNodeConns
    state
    (toSqlKey 1 :: M.NodeId)
    NodeConns
      { ncNodeAgent = Nothing
      , ncNetAgent = Nothing
      , ncSupervisor = stubSup
      , ncVmAutostartFired = vmAS
      , ncNetAutostartFired = netAS
      }
  pure state

-- | Build a fresh test-server state for the current 'TestM'.
withState :: (ServerState -> IO Response) -> TestM Response
withState body = do
  pool <- getDbPool
  tempDir <- getTempDir
  state <- liftIO $ createTestServerState pool tempDir
  resp <- liftIO (body state)
  setLastResponse resp
  pure resp

--------------------------------------------------------------------------------
-- Reference helpers
--------------------------------------------------------------------------------

toRef :: Int64 -> Ref
toRef = Ref . T.pack . show

-- | Resolve a 'Ref' to a 'VmId', failing the test on miss.
resolveVmId :: ServerState -> Ref -> IO Int64
resolveVmId st r = do
  e <- resolveVm r (ssDbPool st)
  case e of
    Right vid -> pure vid
    Left _ -> error ("Test.DSL.When.resolveVmId: VM not found for " <> show r)

resolveDiskId :: ServerState -> Ref -> IO Int64
resolveDiskId st r = do
  e <- resolveDisk r (ssDbPool st)
  case e of
    Right d -> pure d
    Left _ -> error ("Test.DSL.When.resolveDiskId: disk not found for " <> show r)

--------------------------------------------------------------------------------
-- VM Commands
--------------------------------------------------------------------------------

vmList :: TestM [VmInfo]
vmList = do
  resp <- withState handleVmList
  case resp of
    RespVmList vs -> pure vs
    _ -> error ("vmList: unexpected " <> show resp)

vmShow :: Int64 -> TestM (Maybe VmDetails)
vmShow vmId = do
  resp <- withState (`handleVmShow` vmId)
  case resp of
    RespVmDetails d -> pure (Just d)
    RespVmNotFound -> pure Nothing
    _ -> error ("vmShow: unexpected " <> show resp)

vmStart :: Int64 -> TestM Response
vmStart vmId = withState (\st -> runAction st "alice" (VmStart vmId))

vmStop :: Int64 -> TestM Response
vmStop vmId = withState (\st -> runAction st "alice" (VmStop vmId 300))

vmPause :: Int64 -> TestM Response
vmPause vmId = withState (\st -> runAction st "alice" (VmPause vmId))

vmReset :: Int64 -> TestM Response
vmReset vmId = withState (\st -> runAction st "alice" (VmReset vmId))

--------------------------------------------------------------------------------
-- Disk Commands
--------------------------------------------------------------------------------

diskCreate :: Text -> DriveFormat -> Int64 -> TestM Response
diskCreate name format sizeMb =
  withState (\st -> runAction st "alice" (DiskCreate name format sizeMb Nothing False ""))

diskCreateOverlay :: Text -> Int64 -> Maybe Text -> TestM Response
diskCreateOverlay name baseDiskId mPath =
  withState (\st -> runAction st "alice" (DiskCreateOverlay name baseDiskId Nothing mPath False))

diskRegister :: Text -> Text -> DriveFormat -> TestM Response
diskRegister name filePath format =
  withState (\st -> runAction st "alice" (DiskRegister name filePath (Just format) Nothing False ""))

diskRegisterWithBacking :: Text -> Text -> DriveFormat -> Text -> TestM Response
diskRegisterWithBacking name filePath format backingRef =
  withState $ \st -> do
    backingId <- resolveDiskId st (Ref backingRef)
    runAction st "alice" (DiskRegister name filePath (Just format) (Just backingId) False "")

diskClone :: Text -> Int64 -> Maybe Text -> TestM Response
diskClone name baseDiskId mPath =
  withState (\st -> runAction st "alice" (DiskClone name baseDiskId Nothing mPath False))

diskRebase :: Int64 -> Maybe Int64 -> Bool -> TestM Response
diskRebase diskId mNewBackingId unsafe =
  withState (\st -> runAction st "alice" (DiskRebase diskId mNewBackingId unsafe))

diskImport :: Text -> Text -> Maybe Text -> Maybe Text -> Bool -> TestM Response
diskImport name source mPath mFormat _wait =
  withState (\st -> runAction st "alice" (DiskImportAction name source mPath mFormat Nothing False ""))

diskDelete :: Int64 -> TestM Response
diskDelete diskId = withState (\st -> runAction st "alice" (DiskDelete diskId))

diskResize :: Int64 -> Int64 -> TestM Response
diskResize diskId newSizeMb =
  withState (\st -> runAction st "alice" (DiskResize diskId newSizeMb))

diskList :: TestM Response
diskList = withState handleDiskList

diskShow :: Int64 -> TestM Response
diskShow diskId = withState (`handleDiskShow` diskId)

diskAttach :: Int64 -> Int64 -> DriveInterface -> Maybe DriveMedia -> TestM Response
diskAttach vmId diskId interface media =
  withState (\st -> runAction st "alice" (DiskAttach vmId diskId interface media False False CacheWriteback))

diskAttachReadOnly :: Int64 -> Int64 -> DriveInterface -> Maybe DriveMedia -> TestM Response
diskAttachReadOnly vmId diskId interface media =
  withState (\st -> runAction st "alice" (DiskAttach vmId diskId interface media True False CacheNone))

diskDetach :: Int64 -> Int64 -> TestM Response
diskDetach vmId diskId =
  withState (\st -> runAction st "alice" (DiskDetachByDisk vmId diskId))

--------------------------------------------------------------------------------
-- Snapshot Commands
--------------------------------------------------------------------------------

snapshotCreate :: Int64 -> Text -> TestM Response
snapshotCreate diskId name =
  withState
    ( \st ->
        runAction
          st
          "alice"
          (SnapshotCreate diskId name NOA.QuiesceAuto False)
    )

snapshotDelete :: Int64 -> Int64 -> TestM Response
snapshotDelete diskId snapshotId =
  withState (\st -> runAction st "alice" (SnapshotDelete diskId (toRef snapshotId)))

snapshotRollback :: Int64 -> Int64 -> TestM Response
snapshotRollback diskId snapshotId =
  withState (\st -> runAction st "alice" (SnapshotRollback diskId (toRef snapshotId)))

snapshotMerge :: Int64 -> Int64 -> TestM Response
snapshotMerge diskId snapshotId =
  withState (\st -> runAction st "alice" (SnapshotMerge diskId (toRef snapshotId)))

snapshotList :: Int64 -> TestM Response
snapshotList diskId = withState (`handleSnapshotList` diskId)

--------------------------------------------------------------------------------
-- Shared Directory Commands
--------------------------------------------------------------------------------

whenSharedDirAdd :: Int64 -> Text -> Text -> SharedDirCache -> Bool -> TestM Response
whenSharedDirAdd vmId path tag cache readOnly =
  withState (\st -> runAction st "alice" (SharedDirAdd vmId path tag cache readOnly))

whenSharedDirRemove :: Int64 -> Int64 -> TestM Response
whenSharedDirRemove vmId sharedDirId =
  withState (\st -> runAction st "alice" (SharedDirRemove vmId sharedDirId))

whenSharedDirList :: Int64 -> TestM Response
whenSharedDirList vmId = withState (`handleSharedDirList` vmId)

--------------------------------------------------------------------------------
-- Network Interface Commands
--------------------------------------------------------------------------------

whenNetIfAdd :: Int64 -> NetInterfaceType -> Text -> Maybe Text -> TestM Response
whenNetIfAdd vmId ifaceType hostDevice mac =
  withState (\st -> runAction st "alice" (NetIfAdd vmId ifaceType hostDevice mac Nothing))

whenNetIfRemove :: Int64 -> Int64 -> TestM Response
whenNetIfRemove vmId netIfId =
  withState (\st -> runAction st "alice" (NetIfRemove vmId netIfId))

whenNetIfList :: Int64 -> TestM Response
whenNetIfList vmId = withState (`handleNetIfList` vmId)

--------------------------------------------------------------------------------
-- SSH Key Commands
--------------------------------------------------------------------------------

whenSshKeyCreate :: Text -> Text -> TestM Response
whenSshKeyCreate name publicKey =
  withState (\st -> runAction st "alice" (SshKeyCreate name publicKey))

whenSshKeyDelete :: Int64 -> TestM Response
whenSshKeyDelete keyId = withState (\st -> runAction st "alice" (SshKeyDelete keyId))

whenSshKeyList :: TestM Response
whenSshKeyList = withState handleSshKeyList

whenSshKeyAttach :: Int64 -> Int64 -> TestM Response
whenSshKeyAttach vmId keyId = withState (\st -> runAction st "alice" (SshKeyAttach vmId keyId))

whenSshKeyDetach :: Int64 -> Int64 -> TestM Response
whenSshKeyDetach vmId keyId = withState (\st -> runAction st "alice" (SshKeyDetach vmId keyId))

whenSshKeyListForVm :: Int64 -> TestM Response
whenSshKeyListForVm vmId = withState (`handleSshKeyListForVm` vmId)

--------------------------------------------------------------------------------
-- VM Edit / Create / Delete
--------------------------------------------------------------------------------

whenVmEdit :: Int64 -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> TestM Response
whenVmEdit vmId mCpus mRam mDesc mHeadless =
  withState (\st -> runAction st "alice" (VmEdit vmId mCpus mRam mDesc mHeadless Nothing Nothing Nothing Nothing Nothing))

whenVmCreate :: Text -> Int -> Int -> Maybe Text -> TestM Response
whenVmCreate name cpuCount ramMb description =
  -- Empty node ref triggers the scheduler, which picks the
  -- seeded 'test-node' (DB id 1).
  withState (\st -> runAction st "alice" (VmHandlers.VmCreate name "" cpuCount ramMb description False False False False False ""))

whenVmDelete :: Int64 -> TestM Response
whenVmDelete vmId =
  withState (\st -> runAction st "alice" (VmDelete vmId False))

--------------------------------------------------------------------------------
-- Core Commands
--------------------------------------------------------------------------------

whenPing :: TestM Response
whenPing = withState (const handlePing)

whenStatus :: TestM Response
whenStatus = withState handleStatus

whenShutdown :: TestM Response
whenShutdown = withState handleShutdown

--------------------------------------------------------------------------------
-- Apply
--------------------------------------------------------------------------------

whenApply :: Text -> TestM Response
whenApply yaml = withState $ \st -> do
  validated <- handleApplyValidate st yaml
  case validated of
    Left err -> pure err
    Right cfg -> runAction st "alice" (ApplyAction cfg False)

--------------------------------------------------------------------------------
-- Network Commands
--------------------------------------------------------------------------------

whenNetworkCreate :: Text -> Text -> TestM Response
whenNetworkCreate name subnet =
  -- Empty node ref triggers the scheduler.
  withState (\st -> runAction st "alice" (NetworkCreate name "" subnet False False False []))

whenNetworkDelete :: Int64 -> TestM Response
whenNetworkDelete nwId =
  withState (\st -> runAction st "alice" (NetworkDelete nwId))

whenNetworkList :: TestM Response
whenNetworkList = withState handleNetworkList

whenNetworkShow :: Int64 -> TestM Response
whenNetworkShow nwId = withState (`handleNetworkShow` nwId)

-- | Toggle the persistent 'netdDisabled' flag on a node, exercising
-- the full 'NodeEdit' action path (including the pre-condition that
-- blocks the flip while netd-dependent resources still reference
-- the node).
whenNodeEditNetdDisabled :: Int64 -> Bool -> TestM Response
whenNodeEditNetdDisabled nodeId disabled =
  withState
    ( \st ->
        runAction
          st
          "alice"
          ( NodeEdit
              { nedNodeId = nodeId
              , nedName = Nothing
              , nedHost = Nothing
              , nedNodeAgentPort = Nothing
              , nedNetAgentPort = Nothing
              , nedBasePath = Nothing
              , nedDescription = Nothing
              , nedAdminState = Nothing
              , nedNetdDisabled = Just disabled
              }
          )
    )

--------------------------------------------------------------------------------
-- Template Commands
--------------------------------------------------------------------------------

whenTemplateCreate :: Text -> TestM Response
whenTemplateCreate yaml = withState (\st -> runAction st "alice" (TemplateCreate yaml))

whenTemplateDelete :: Int64 -> TestM Response
whenTemplateDelete tplId = withState (\st -> runAction st "alice" (TemplateDelete tplId))

whenTemplateList :: TestM Response
whenTemplateList = withState handleTemplateList

whenTemplateShow :: Int64 -> TestM Response
whenTemplateShow tplId = withState (`handleTemplateShow` tplId)

whenTemplateUpdate :: Int64 -> Text -> TestM Response
whenTemplateUpdate tplId yaml = withState (\st -> runAction st "alice" (TemplateUpdate tplId yaml))

whenTemplateInstantiate :: Int64 -> Text -> TestM Response
whenTemplateInstantiate tplId name =
  -- Empty node ref → scheduler picks (the seeded test-node).
  withState (\st -> runAction st "alice" (TemplateInstantiate tplId name ""))

--------------------------------------------------------------------------------
-- Guest Exec
--------------------------------------------------------------------------------

whenGuestExec :: Int64 -> Text -> TestM Response
whenGuestExec vmId cmd = withState (\st -> runAction st "alice" (GuestExec vmId cmd))

--------------------------------------------------------------------------------
-- Cloud-init Config Commands
--------------------------------------------------------------------------------

whenCloudInitSet :: Int64 -> Maybe Text -> Maybe Text -> Bool -> TestM Response
whenCloudInitSet vmId mUserData mNetworkConfig injectSshKeys =
  withState (\st -> runAction st "alice" (CloudInitSet vmId mUserData mNetworkConfig injectSshKeys))

whenCloudInitGet :: Int64 -> TestM Response
whenCloudInitGet vmId = withState (`handleCloudInitGet` vmId)

whenCloudInitDelete :: Int64 -> TestM Response
whenCloudInitDelete vmId = withState (\st -> runAction st "alice" (CloudInitDelete vmId))
