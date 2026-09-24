{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Vm.Configure
  ( VmCreate (..)
  , VmEdit (..)
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
import Corvus.Handlers.Vm.Delete (deleteTpmStateForVm)
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

handleVmCreate
  :: ServerState
  -> Text
  -- ^ name
  -> Text
  -- ^ node ref (name or id); empty = defer to scheduler
  -> Int
  -> Int
  -> Maybe Text
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -- ^ rebootQuirk
  -> Text
  -- ^ cpuModel (empty == "host")
  -> IO Response
handleVmCreate state name nodeRefText cpuCount ramMb description headless guestAgent tpm cloudInit autostart rebootQuirk cpuModel0 =
  case validateName "VM" name of
    Left err -> pure $ RespError err
    Right () -> do
      let pool = ssDbPool state
          -- Empty wire string is the operator's "use the default"
          -- signal — fall back to "host" so existing callers and
          -- older wire clients keep working unchanged.
          cpuModel = if T.null cpuModel0 then "host" else cpuModel0
          placeOn nodeKey = do
            -- Try to allocate a CID via the target node's agent.
            -- A 'Left' here typically means the agent's host has no
            -- vhost-vsock support (or the agent is unreachable);
            -- fall back to creating the VM with vsockCid = Nothing
            -- — QEMU will start without a vhost-vsock-pci device
            -- and operators just lose the @ssh user\@vsock/CID@
            -- shortcut for that VM.
            eVmId <- do
              r <-
                withAllocatedVsockCid state nodeKey $ \cid ->
                  runSqlPool
                    (createVm name nodeKey cpuCount ramMb description headless guestAgent tpm cloudInit autostart rebootQuirk cpuModel (Just cid))
                    pool
              case r of
                Right vmId -> pure (Right vmId)
                Left _ -> do
                  vmId <-
                    runSqlPool
                      (createVm name nodeKey cpuCount ramMb description headless guestAgent tpm cloudInit autostart rebootQuirk cpuModel Nothing)
                      pool
                  pure (Right vmId)
            case eVmId of
              Left err -> pure $ RespError err
              Right vmId -> do
                -- Bump the scheduler's in-memory reservation so the
                -- next 'pickNodeForVm' call (within the same daemon,
                -- before the agent's next stats push) doesn't
                -- double-spend this VM's RAM share. The reservation
                -- clears when fresh 'NodeStats' arrive (Phase 5).
                reserveRam state nodeKey ramMb
                pure $ RespVmCreated vmId
      -- Empty text == operator did not pass @--node@; capnp's
      -- unset-EntityRef default ('byId 0') also lands here.
      -- Either way, defer to the scheduler.
      if T.null nodeRefText || nodeRefText == "0"
        then do
          eNid <- pickNodeForVm state ramMb
          case eNid of
            Left err -> pure $ RespError err
            Right nodeKey -> placeOn nodeKey
        else do
          r <- resolveNode (Ref nodeRefText) pool
          case r of
            Left (RefNotFound _ _) -> pure RespNodeNotFound
            Left re -> pure $ RespAmbiguousRef (resolveErrorMessage re)
            Right nidRaw -> placeOn (M.toSqlKey nidRaw)

-- | Handle VM delete command. Reaps ephemeral disks attached to the
-- VM (cloud-init ISOs, template-instantiated disks) unless 'keepDisks'
-- is set. With 'force', hard-reset first and delete only after reset
handleVmEdit
  :: ServerState
  -> Int64
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -- ^ rebootQuirk
  -> Maybe Text
  -- ^ cpuModel
  -> IO Response
handleVmEdit state vmId mCpus mRam mDesc mHeadless mGuestAgent mTpm mCloudInit mAutostart mRebootQuirk mCpuModel = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (vm, status) ->
      -- 'rebootQuirk' and 'cpuModel' are consumed only at the next
      -- 'vmStart' (via 'VmSpec'), so flipping them on a running VM
      -- has no effect until the next start; allow it without
      -- forcing a stop — matches 'autostart's relaxed-edit
      -- semantics.
      let hasRuntimeEdits =
            or
              [ isJust mCpus
              , isJust mRam
              , isJust mDesc
              , isJust mHeadless
              , isJust mGuestAgent
              , isJust mTpm
              , isJust mCloudInit
              ]
       in if hasRuntimeEdits && status /= VmStopped
            then pure RespVmMustBeStopped
            else do
              tpmDeleteResult <-
                if vmTpm vm && mTpm == Just False
                  then deleteTpmStateForVm state vmId (vmName vm)
                  else pure (Right ())
              case tpmDeleteResult of
                Left err -> pure (RespError err)
                Right () -> do
                  runSqlPool
                    ( editVm
                        vmId
                        mCpus
                        mRam
                        mDesc
                        mHeadless
                        mGuestAgent
                        mTpm
                        mCloudInit
                        mAutostart
                        mRebootQuirk
                        mCpuModel
                    )
                    (ssDbPool state)
                  pure RespVmEdited

-- | Insert a VM record and allocate its VSOCK CID.
--
-- 'handleVmCreate' is responsible for resolving the node ref
-- (or deferring to the scheduler) before invoking this.
createVm
  :: Text
  -> M.NodeId
  -> Int
  -> Int
  -> Maybe Text
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -- ^ rebootQuirk
  -> Text
  -- ^ cpuModel
  -> Maybe Int
  -> SqlPersistT IO Int64
createVm name nodeKey cpuCount ramMb description headless guestAgent tpm cloudInit autostart rebootQuirk cpuModel vsockCid = do
  now <- liftIO getCurrentTime
  let vm =
        Vm
          { vmName = name
          , vmNodeId = nodeKey
          , vmCreatedAt = now
          , vmStatus = VmStopped
          , vmLifecycleRevision = 0
          , vmRuntimeGeneration = Nothing
          , vmCpuCount = cpuCount
          , vmRamMb = ramMb
          , vmDescription = description
          , vmHeadless = headless
          , vmGuestAgent = guestAgent
          , vmTpm = tpm
          , vmCloudInit = cloudInit
          , vmHealthcheck = Nothing
          , vmAutostart = autostart
          , vmSpicePort = Nothing
          , vmVsockCid = vsockCid
          , vmErrorMessage = Nothing
          , vmLastErrorAt = Nothing
          , vmRebootQuirk = rebootQuirk
          , vmCpuModel = cpuModel
          }
  key <- insert vm
  pure $ fromSqlKey key

-- | Edit VM properties. Only updates fields that are Just.
editVm
  :: Int64
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Text
  -> SqlPersistT IO ()
editVm vmId mCpus mRam mDesc mHeadless mGuestAgent mTpm mCloudInit mAutostart mRebootQuirk mCpuModel = do
  let key = toSqlKey vmId :: VmId
      updates =
        maybe [] (\cpus -> [M.VmCpuCount =. cpus]) mCpus
          ++ maybe [] (\ram -> [M.VmRamMb =. ram]) mRam
          ++ maybe [] (\desc -> [M.VmDescription =. Just desc]) mDesc
          ++ maybe [] (\h -> [M.VmHeadless =. h]) mHeadless
          ++ maybe [] (\ga -> [M.VmGuestAgent =. ga]) mGuestAgent
          ++ maybe [] (\tpm -> [M.VmTpm =. tpm]) mTpm
          ++ maybe [] (\ci -> [M.VmCloudInit =. ci]) mCloudInit
          ++ maybe [] (\a -> [M.VmAutostart =. a]) mAutostart
          ++ maybe [] (\rq -> [M.VmRebootQuirk =. rq]) mRebootQuirk
          ++ maybe [] (\cm -> [M.VmCpuModel =. cm]) mCpuModel
  case updates of
    [] -> pure ()
    us -> update key us

data VmCreate = VmCreate
  { vcrName :: Text
  , vcrNodeRef :: Text
  -- ^ Reference to the target node (name or numeric id). Empty
  -- string / @"0"@ defers to
  -- 'Corvus.Handlers.Scheduler.pickNodeForVm'; non-empty is
  -- resolved by 'handleVmCreate'.
  , vcrCpuCount :: Int
  , vcrRamMb :: Int
  , vcrDescription :: Maybe Text
  , vcrHeadless :: Bool
  , vcrGuestAgent :: Bool
  , vcrTpm :: Bool
  , vcrCloudInit :: Bool
  , vcrAutostart :: Bool
  , vcrRebootQuirk :: Bool
  , vcrCpuModel :: Text
  -- ^ QEMU @-cpu@ model. Empty == use the daemon default
  -- ('host'); see the schema field comment on
  -- @schema/vm.capnp::VmInfo.cpuModel@ for the
  -- migration-safety trade-off.
  }

instance Action VmCreate where
  actionSubsystem _ = SubVm
  actionCommand _ = "create"
  actionEntityName = Just . vcrName
  actionExecute ctx a =
    handleVmCreate
      (acState ctx)
      (vcrName a)
      (vcrNodeRef a)
      (vcrCpuCount a)
      (vcrRamMb a)
      (vcrDescription a)
      (vcrHeadless a)
      (vcrGuestAgent a)
      (vcrTpm a)
      (vcrCloudInit a)
      (vcrAutostart a)
      (vcrRebootQuirk a)
      (vcrCpuModel a)

data VmEdit = VmEdit
  { vedVmId :: Int64
  , vedCpus :: Maybe Int
  , vedRam :: Maybe Int
  , vedDesc :: Maybe Text
  , vedHeadless :: Maybe Bool
  , vedGuestAgent :: Maybe Bool
  , vedTpm :: Maybe Bool
  , vedCloudInit :: Maybe Bool
  , vedAutostart :: Maybe Bool
  , vedRebootQuirk :: Maybe Bool
  , vedCpuModel :: Maybe Text
  }

instance Action VmEdit where
  actionSubsystem _ = SubVm
  actionCommand _ = "edit"
  actionEntityId = Just . fromIntegral . vedVmId
  actionExecute ctx a =
    handleVmEdit
      (acState ctx)
      (vedVmId a)
      (vedCpus a)
      (vedRam a)
      (vedDesc a)
      (vedHeadless a)
      (vedGuestAgent a)
      (vedTpm a)
      (vedCloudInit a)
      (vedAutostart a)
      (vedRebootQuirk a)
      (vedCpuModel a)
