{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Vm.Query
  ( handleVmList
  , handleVmShow
  , listVms
  , getVmDetails
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Disk (DiskDelete (..))
import Corvus.Handlers.Disk.Db (diskImageNodeFilePathFor)
import Corvus.Handlers.Resolve (ResolveError (..), resolveErrorMessage, resolveNode, validateName)
import Corvus.Handlers.Scheduler (pickNodeForVm)
import Corvus.Handlers.Vm.CloudInit (ensureCloudInitIso)
import Corvus.Handlers.Vm.Console (generateSpicePassword)
import Corvus.Handlers.Vm.Db
import Corvus.Handlers.Vm.Monitor (attachVmMonitor, releaseManagedTaps)
import Corvus.Model (DriveFormat (..), VmStatus (..))
import Corvus.Model hiding (DriveFormat, VmStatus)
import qualified Corvus.Model as M
import Corvus.Model.VmState (VmAction (..), validateTransition)
import Corvus.Node.SpicePort (withAllocatedSpicePort)
import Corvus.Node.VsockCid (withAllocatedVsockCid)
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.NodeAgentClient.Spec as NSpec
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol
import Corvus.Qemu (QemuConfig, getGuestAgentSocket, getMonitorSocket, getSerialSocket)
import Corvus.Types
import Data.Int (Int64)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Word (Word32)
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlPool)
import System.FilePath ((</>))

handleVmList :: ServerState -> IO Response
handleVmList state = do
  vms <- runSqlPool listVms (ssDbPool state)
  pure $ RespVmList vms

-- | Handle VM show command
handleVmShow :: ServerState -> Int64 -> IO Response
handleVmShow state vmId = do
  result <- runSqlPool (getVmDetails (ssQemuConfig state) vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just details -> pure $ RespVmDetails details

listVms :: SqlPersistT IO [VmInfo]
listVms = do
  vms <- selectList [] [Asc M.VmName]
  -- Bulk-fetch the nodes so we can stamp the display name on each
  -- VM row without an N+1 of single-row gets. Empty fallback for
  -- a (rare) Vm whose Node row has been deleted under it.
  nodes <- selectList [] []
  let nameOf = Map.fromList [(entityKey n, M.nodeName (entityVal n)) | n <- nodes]
  pure $ map (toVmInfo nameOf) vms
  where
    toVmInfo nameOf (Entity key vm) =
      VmInfo
        { viId = fromSqlKey key
        , viName = vmName vm
        , viNode =
            NamedRef
              { nrId = fromSqlKey (vmNodeId vm)
              , nrName = Map.findWithDefault "(deleted)" (vmNodeId vm) nameOf
              }
        , viStatus = vmStatus vm
        , viCpuCount = vmCpuCount vm
        , viRamMb = vmRamMb vm
        , viHeadless = vmHeadless vm
        , viGuestAgent = vmGuestAgent vm
        , viTpm = vmTpm vm
        , viCloudInit = vmCloudInit vm
        , viHealthcheck = vmHealthcheck vm
        , viAutostart = vmAutostart vm
        , viRebootQuirk = vmRebootQuirk vm
        , viCpuModel = vmCpuModel vm
        }

-- | Get full VM details. Re-exported so 'Corvus.Handlers.Build' can
-- read the bake VM and expose its identity to provisioner shell steps
-- (see @CORVUS_BAKEVM*@ environment variables).
getVmDetails :: QemuConfig -> Int64 -> SqlPersistT IO (Maybe VmDetails)
getVmDetails config vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  case mVm of
    Nothing -> pure Nothing
    Just vm -> do
      drives <- selectList [M.DriveVmId ==. key] [Asc M.DriveId]
      netIfs <- selectList [M.NetworkInterfaceVmId ==. key] []
      -- Look up the node name for display. Sentinel on a missing
      -- row (race against a node delete) matches 'listVms'.
      mNode <- get (vmNodeId vm)
      let nodeName' = maybe "(deleted)" M.nodeName mNode
          -- VM's node basePath is used to absolutise relative
          -- DiskImageNode paths for the user-facing 'DriveInfo'
          -- — same convention as 'handleDiskShow' (drives lie on
          -- the VM's node, so anchor against its basePath, not
          -- the daemon's).
          vmNodeBasePath = maybe "" (T.unpack . M.nodeBasePath) mNode
      -- Get socket paths
      monitorSock <- liftIO $ getMonitorSocket config vmId
      serialSock <- liftIO $ getSerialSocket config vmId
      guestAgentSock <- liftIO $ getGuestAgentSocket config vmId
      -- Build drive info by fetching disk images
      driveInfos <- mapM (toDriveInfo (vmNodeId vm) vmNodeBasePath) drives
      -- Build NIC info by resolving each NIC's network reference
      netIfInfos <- mapM toNetIfInfo netIfs
      -- Get custom cloud-init config if present
      mCiConfig <- getBy (M.UniqueCloudInitVm key)
      let ciInfo =
            fmap
              ( \(Entity _ ci) ->
                  CloudInitInfo
                    { ciiUserData = cloudInitUserData ci
                    , ciiNetworkConfig = cloudInitNetworkConfig ci
                    , ciiInjectSshKeys = cloudInitInjectSshKeys ci
                    }
              )
              mCiConfig
      pure $
        Just
          VmDetails
            { vdId = vmId
            , vdName = vmName vm
            , vdNode = NamedRef {nrId = fromSqlKey (vmNodeId vm), nrName = nodeName'}
            , vdCreatedAt = vmCreatedAt vm
            , vdStatus = vmStatus vm
            , vdCpuCount = vmCpuCount vm
            , vdRamMb = vmRamMb vm
            , vdDescription = vmDescription vm
            , vdDrives = driveInfos
            , vdNetIfs = netIfInfos
            , vdHeadless = vmHeadless vm
            , vdMonitorSocket = T.pack monitorSock
            , vdSpicePort = vmSpicePort vm
            , vdVsockCid = vmVsockCid vm
            , vdSerialSocket = T.pack serialSock
            , vdGuestAgentSocket = T.pack guestAgentSock
            , vdGuestAgent = vmGuestAgent vm
            , vdTpm = vmTpm vm
            , vdCloudInit = vmCloudInit vm
            , vdCloudInitConfig = ciInfo
            , vdHealthcheck = vmHealthcheck vm
            , vdAutostart = vmAutostart vm
            , vdErrorMessage = vmErrorMessage vm
            , vdLastErrorAt = vmLastErrorAt vm
            , vdRebootQuirk = vmRebootQuirk vm
            , vdCpuModel = vmCpuModel vm
            , vdStats = Corvus.Protocol.zeroVmStats
            }
  where
    -- \^ Daemon handlers don't fill vdStats; the RPC layer
    -- looks up the latest sample from 'ssVmStatsRing' and
    -- threads it into 'toCapnpVmDetails' separately.

    toDriveInfo vmNode vmNodeBasePath (Entity driveKey drive) = do
      case driveDiskImageId drive of
        -- Ejected media drive: the tray is empty and no 'DiskImage'
        -- is attached.
        Nothing ->
          pure
            DriveInfo
              { diId = fromSqlKey driveKey
              , diDiskImage = Nothing
              , diInterface = driveInterface drive
              , diFilePath = T.empty
              , diFormat = FormatRaw
              , diMedia = driveMedia drive
              , diReadOnly = driveReadOnly drive
              , diCacheType = driveCacheType drive
              , diDiscard = driveDiscard drive
              }
        Just diskImageKey -> do
          mDiskImage <- get diskImageKey
          case mDiskImage of
            Nothing ->
              pure
                DriveInfo
                  { diId = fromSqlKey driveKey
                  , diDiskImage =
                      Just (NamedRef {nrId = fromSqlKey diskImageKey, nrName = "(deleted)"})
                  , diInterface = driveInterface drive
                  , diFilePath = "(deleted)"
                  , diFormat = FormatRaw
                  , diMedia = driveMedia drive
                  , diReadOnly = driveReadOnly drive
                  , diCacheType = driveCacheType drive
                  , diDiscard = driveDiscard drive
                  }
            Just diskImage -> do
              -- Resolve the file path from the DiskImageNode row for
              -- the VM's node — single-node deployments produce exactly
              -- one row, multi-node deployments resolve to the path on
              -- the VM's host. Stored form is relative-to-basePath
              -- (or absolute when registered outside basePath); we
              -- absolutise here against the VM's node basePath so the
              -- DTO matches what 'disks.show()' returns. Missing row
              -- yields the empty string, which the CLI renders as
              -- "(not present)".
              mPath <- diskImageNodeFilePathFor diskImageKey vmNode
              let absPath = case mPath of
                    Nothing -> T.empty
                    Just stored ->
                      let raw = T.unpack stored
                       in if "/" `isPrefixOf` raw
                            then stored
                            else T.pack (vmNodeBasePath </> raw)
              pure
                DriveInfo
                  { diId = fromSqlKey driveKey
                  , diDiskImage =
                      Just
                        ( NamedRef
                            { nrId = fromSqlKey diskImageKey
                            , nrName = diskImageName diskImage
                            }
                        )
                  , diInterface = driveInterface drive
                  , diFilePath = absPath
                  , diFormat = diskImageFormat diskImage
                  , diMedia = driveMedia drive
                  , diReadOnly = driveReadOnly drive
                  , diCacheType = driveCacheType drive
                  , diDiscard = driveDiscard drive
                  }
    toNetIfInfo (Entity netIfKey netIf) = do
      networkRef <- case networkInterfaceNetworkId netIf of
        Nothing -> pure Nothing
        Just nwKey -> do
          mNw <- get nwKey
          pure $
            fmap
              (\nw -> NamedRef {nrId = fromSqlKey nwKey, nrName = networkName nw})
              mNw
      pure
        NetIfInfo
          { niId = fromSqlKey netIfKey
          , niType = networkInterfaceInterfaceType netIf
          , niHostDevice = networkInterfaceHostDevice netIf
          , niMacAddress = networkInterfaceMacAddress netIf
          , niNetwork = networkRef
          , niGuestIpAddresses = networkInterfaceGuestIpAddresses netIf
          , niIpAddress = networkInterfaceIpAddress netIf
          }

-- | Check if all networks referenced by a VM's network interfaces are running.
