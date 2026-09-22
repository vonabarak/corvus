{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | VM-related RPC wrappers extracted from "Corvus.Client.Capnp.Rpc".
module Corvus.Client.Capnp.Rpc.Vm
  ( -- * VM read methods
    rpcVmList
  , rpcVmShow

    -- * VM lifecycle
  , rpcVmCreate
  , rpcVmStart
  , rpcVmStop
  , rpcVmPause
  , rpcVmReset
  , rpcVmSave
  , rpcVmDelete
  , rpcVmMigrate

    -- * VM additional wrappers
  , rpcVmEdit
  , rpcVmCloudInit
  , rpcVmViewGrant
  , rpcVmSendCtrlAltDel
  , rpcVmSerialConsoleFlush
  , rpcVmHmpMonitorFlush
  , rpcGuestExec

    -- * VM-scoped snapshots
  , rpcVmSnapshotCreate
  , rpcVmSnapshotList
  , rpcVmSnapshotRollback
  , rpcVmSnapshotDelete

    -- * Shared directory
  , rpcSharedDirAdd
  , rpcSharedDirRemove
  , rpcSharedDirList

    -- * Network interface (per-VM)
  , rpcNetIfAdd
  , rpcNetIfRemove
  , rpcNetIfList

    -- * SSH key attach / detach / list-for-vm
  , rpcSshKeyAttach
  , rpcSshKeyDetach
  , rpcSshKeyListForVm

    -- * VM streaming
  , rpcVmSerialConsole
  , rpcVmHmpMonitor
  , streamByteSinkMethod

    -- * Guest-agent subscription
  , rpcVmSubscribeGuestAgent
  , GuestAgentStatusEvent (..)

    -- * Helpers
  , getVmClient
  )
where

import Capnp (export)
import qualified Capnp as C
import qualified Capnp.Gen.Cloudinit as CGCI
import qualified Capnp.Gen.Common as CGCommon
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Network as CGNet
import qualified Capnp.Gen.Sshkey as CGSsh
import qualified Capnp.Gen.Streams as CGS
import qualified Capnp.Gen.Vm as CGVm
import Capnp.Rpc.Server (SomeServer, handleParsed)
import qualified Capnp.Rpc.Untyped (nullClient)
import Control.Exception (SomeException, try)
import qualified Control.Monad
import Corvus.Client.Capnp.Connection (CapnpConnection (..))
import Corvus.Model
  ( CacheType
  , DriveMedia (..)
  , NetInterfaceType
  , SharedDirCache
  )
import qualified Corvus.Protocol.CloudInit as PCI
import qualified Corvus.Protocol.SharedDir as PSd
import qualified Corvus.Protocol.SshKey as PSk
import qualified Corvus.Protocol.Vm as PV
import Corvus.Wire.CloudInit (fromCapnpCloudInitInfo, toCapnpCloudInitInfo)
import Corvus.Wire.Common (EntityRef, ViewGrant (..), entityRefFromText, fromCapnpViewGrant, toCapnpEntityRef)
import Corvus.Wire.Enums
  ( toCapnpCacheType
  , toCapnpDriveMedia
  , toCapnpNetInterfaceType
  , toCapnpSharedDirCache
  )
import Corvus.Wire.Errors (WireError, showWireError)
import qualified Corvus.Wire.Network as WNet
import qualified Corvus.Wire.SharedDir as WSd
import qualified Corvus.Wire.SshKey as WSsh
import qualified Corvus.Wire.Vm as WVm
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)

-- | Call a method on a cap and return its parsed results struct.
callOn
  :: ( C.IsCap iface
     , C.IsStruct params
     , C.IsStruct results
     , C.Parse params (C.Parsed params)
     , C.Parse results (C.Parsed results)
     )
  => C.Method iface params results
  -> C.Parsed params
  -> C.Client iface
  -> IO (C.Parsed results)
callOn method p client = do
  raw <- (client & C.callP method p) >>= C.waitPipeline
  C.evalLimitT C.defaultLimit (C.parse raw)

failOnWire :: Either WireError a -> IO a
failOnWire (Right a) = pure a
failOnWire (Left e) = fail ("wire decode error: " <> show (showWireError e))

-- ---------------------------------------------------------------------
-- VM read methods
-- ---------------------------------------------------------------------

rpcVmList :: CapnpConnection -> IO [PV.VmInfo]
rpcVmList conn = do
  CGCorvus.Daemon'vms'results {CGCorvus.mgr = mgr} <-
    callOn #vms CGCorvus.Daemon'vms'params (ccDaemon conn)
  CGVm.VmManager'list'results {CGVm.vms = vms} <-
    callOn #list CGVm.VmManager'list'params mgr
  traverse (failOnWire . WVm.fromCapnpVmInfo) vms

rpcVmShow :: CapnpConnection -> EntityRef -> IO PV.VmDetails
rpcVmShow conn refIn = do
  CGCorvus.Daemon'vms'results {CGCorvus.mgr = mgr} <-
    callOn #vms CGCorvus.Daemon'vms'params (ccDaemon conn)
  CGVm.VmManager'get'results {CGVm.vm = vmClient} <-
    callOn #get CGVm.VmManager'get'params {CGVm.ref = toCapnpEntityRef refIn} mgr
  CGVm.Vm'show'results {CGVm.details = det} <-
    callOn #show CGVm.Vm'show'params vmClient
  (details, _sharedDirs) <- failOnWire (WVm.fromCapnpVmDetails det)
  pure details

-- ---------------------------------------------------------------------
-- VM lifecycle
-- ---------------------------------------------------------------------

rpcVmCreate
  :: CapnpConnection
  -> Text
  -- ^ name
  -> Text
  -- ^ node reference (name or numeric id)
  -> Int
  -- ^ cpus
  -> Int
  -- ^ ram (MB)
  -> Maybe Text
  -- ^ description
  -> Bool
  -- ^ headless
  -> Bool
  -- ^ guest agent
  -> Bool
  -- ^ TPM 2.0
  -> Bool
  -- ^ cloud-init
  -> Bool
  -- ^ autostart
  -> Bool
  -- ^ rebootQuirk
  -> Text
  -- ^ cpuModel
  -> IO Int64
rpcVmCreate conn name nodeRef cpus ram desc headless ga tpm ci autostart rq cm = do
  CGCorvus.Daemon'vms'results {CGCorvus.mgr = mgr} <-
    callOn #vms CGCorvus.Daemon'vms'params (ccDaemon conn)
  let inner =
        CGVm.VmCreateParams
          { CGVm.name = name
          , CGVm.node = toCapnpEntityRef (entityRefFromText nodeRef)
          , CGVm.cpuCount = fromIntegral cpus
          , CGVm.ramMb = fromIntegral ram
          , CGVm.description = fromMaybe "" desc
          , CGVm.headless = headless
          , CGVm.guestAgent = ga
          , CGVm.tpm = tpm
          , CGVm.cloudInit = ci
          , CGVm.autostart = autostart
          , CGVm.rebootQuirk = rq
          , CGVm.cpuModel = cm
          }
  CGVm.VmManager'create'results {CGVm.vm = vmClient} <-
    callOn #create CGVm.VmManager'create'params {CGVm.params = inner} mgr
  CGVm.Vm'show'results {CGVm.details = det} <-
    callOn #show CGVm.Vm'show'params vmClient
  case det of
    CGVm.VmDetails {CGVm.id = vid} -> pure vid

rpcVmStart :: CapnpConnection -> EntityRef -> Bool -> IO ()
rpcVmStart conn ref wait = do
  vmClient <- getVmClient conn ref
  _ <- callOn #start CGVm.Vm'start'params {CGVm.wait = wait} vmClient
  pure ()

rpcVmStop :: CapnpConnection -> EntityRef -> Bool -> Word32 -> IO ()
rpcVmStop conn ref wait timeoutSec = do
  vmClient <- getVmClient conn ref
  _ <-
    callOn
      #stop
      CGVm.Vm'stop'params {CGVm.wait = wait, CGVm.timeoutSec = timeoutSec}
      vmClient
  pure ()

rpcVmPause :: CapnpConnection -> EntityRef -> IO ()
rpcVmPause conn ref = do
  vmClient <- getVmClient conn ref
  _ <- callOn #pause CGVm.Vm'pause'params vmClient
  pure ()

rpcVmReset :: CapnpConnection -> EntityRef -> IO ()
rpcVmReset conn ref = do
  vmClient <- getVmClient conn ref
  _ <- callOn #reset CGVm.Vm'reset'params vmClient
  pure ()

rpcVmSave :: CapnpConnection -> EntityRef -> Bool -> IO ()
rpcVmSave conn ref wait = do
  vmClient <- getVmClient conn ref
  _ <- callOn #save CGVm.Vm'save'params {CGVm.wait = wait} vmClient
  pure ()

rpcVmDelete :: CapnpConnection -> EntityRef -> Bool -> Bool -> IO ()
rpcVmDelete conn ref keepDisks force = do
  vmClient <- getVmClient conn ref
  _ <- callOn #delete CGVm.Vm'delete'params {CGVm.keepDisks = keepDisks, CGVm.force = force} vmClient
  pure ()

rpcVmMigrate :: CapnpConnection -> EntityRef -> EntityRef -> IO Int64
rpcVmMigrate conn vmRef toNodeRef = do
  vmClient <- getVmClient conn vmRef
  let p = CGVm.VmMigrateParams {CGVm.toNodeRef = toCapnpEntityRef toNodeRef}
  CGVm.Vm'migrate'results {CGVm.taskId = tid} <-
    callOn #migrate CGVm.Vm'migrate'params {CGVm.params = p} vmClient
  pure tid

-- ---------------------------------------------------------------------
-- VM additional wrappers
-- ---------------------------------------------------------------------

rpcVmEdit
  :: CapnpConnection
  -> EntityRef
  -> Maybe Int
  -- ^ new cpus
  -> Maybe Int
  -- ^ new ram (MB)
  -> Maybe Text
  -- ^ new description
  -> Maybe Bool
  -- ^ new headless
  -> Maybe Bool
  -- ^ new guest agent
  -> Maybe Bool
  -- ^ new TPM setting
  -> Maybe Bool
  -- ^ new cloud-init
  -> Maybe Bool
  -- ^ new autostart
  -> Maybe Bool
  -- ^ new rebootQuirk
  -> Maybe Text
  -- ^ new cpuModel
  -> IO ()
rpcVmEdit conn ref mCpus mRam mDesc mHeadless mGa mTpm mCi mAs mRq mCm = do
  vmClient <- getVmClient conn ref
  let p =
        CGVm.VmEditParams
          { CGVm.hasName = False
          , CGVm.name = ""
          , CGVm.hasCpuCount = isJust mCpus
          , CGVm.cpuCount = maybe 0 fromIntegral mCpus
          , CGVm.hasRamMb = isJust mRam
          , CGVm.ramMb = maybe 0 fromIntegral mRam
          , CGVm.hasDescription = isJust mDesc
          , CGVm.description = fromMaybe "" mDesc
          , CGVm.hasHeadless = isJust mHeadless
          , CGVm.headless = fromMaybe False mHeadless
          , CGVm.hasGuestAgent = isJust mGa
          , CGVm.guestAgent = fromMaybe False mGa
          , CGVm.hasTpm = isJust mTpm
          , CGVm.tpm = fromMaybe False mTpm
          , CGVm.hasCloudInit = isJust mCi
          , CGVm.cloudInit = fromMaybe False mCi
          , CGVm.hasAutostart = isJust mAs
          , CGVm.autostart = fromMaybe False mAs
          , CGVm.hasRebootQuirk = isJust mRq
          , CGVm.rebootQuirk = fromMaybe False mRq
          , CGVm.hasCpuModel = isJust mCm
          , CGVm.cpuModel = fromMaybe "" mCm
          }
  _ <- callOn #edit CGVm.Vm'edit'params {CGVm.params = p} vmClient
  pure ()

rpcVmCloudInit :: CapnpConnection -> EntityRef -> IO (Maybe PCI.CloudInitInfo)
rpcVmCloudInit conn ref = do
  vmClient <- getVmClient conn ref
  CGVm.Vm'cloudInit'results {CGVm.config = cfg} <-
    callOn #cloudInit CGVm.Vm'cloudInit'params vmClient
  let parsed = fromCapnpCloudInitInfo cfg
  case parsed of
    PCI.CloudInitInfo {PCI.ciiUserData = Nothing, PCI.ciiNetworkConfig = Nothing, PCI.ciiInjectSshKeys = False} ->
      pure Nothing
    other -> pure (Just other)

rpcVmViewGrant :: CapnpConnection -> EntityRef -> IO ViewGrant
rpcVmViewGrant conn ref = do
  vmClient <- getVmClient conn ref
  CGVm.Vm'viewGrant'results {CGVm.grant = g} <-
    callOn #viewGrant CGVm.Vm'viewGrant'params vmClient
  pure (fromCapnpViewGrant g)

rpcVmSendCtrlAltDel :: CapnpConnection -> EntityRef -> IO ()
rpcVmSendCtrlAltDel conn ref = do
  vmClient <- getVmClient conn ref
  _ <- callOn #sendCtrlAltDel CGVm.Vm'sendCtrlAltDel'params vmClient
  pure ()

rpcVmSerialConsoleFlush :: CapnpConnection -> EntityRef -> IO ()
rpcVmSerialConsoleFlush conn ref = do
  vmClient <- getVmClient conn ref
  _ <- callOn #serialConsoleFlush CGVm.Vm'serialConsoleFlush'params vmClient
  pure ()

rpcVmHmpMonitorFlush :: CapnpConnection -> EntityRef -> IO ()
rpcVmHmpMonitorFlush conn ref = do
  vmClient <- getVmClient conn ref
  _ <- callOn #hmpMonitorFlush CGVm.Vm'hmpMonitorFlush'params vmClient
  pure ()

rpcGuestExec :: CapnpConnection -> EntityRef -> Text -> IO (Int, Text, Text)
rpcGuestExec conn ref cmd = do
  vmClient <- getVmClient conn ref
  CGVm.Vm'guestExec'results {CGVm.result = CGVm.GuestExecResult {..}} <-
    callOn #guestExec CGVm.Vm'guestExec'params {CGVm.command = cmd} vmClient
  pure (fromIntegral exitCode, stdout, stderr)

-- ---------------------------------------------------------------------
-- VM-scoped snapshots
-- ---------------------------------------------------------------------

rpcVmSnapshotCreate
  :: CapnpConnection -> EntityRef -> Text -> IO PV.VmSnapshotInfo
rpcVmSnapshotCreate conn vmRef name = do
  vClient <- getVmClient conn vmRef
  CGVm.Vm'snapshotCreate'results {CGVm.info = info} <-
    callOn
      #snapshotCreate
      CGVm.Vm'snapshotCreate'params {CGVm.name = name}
      vClient
  pure (WVm.fromCapnpVmSnapshotInfo info)

rpcVmSnapshotList :: CapnpConnection -> EntityRef -> IO [PV.VmSnapshotInfo]
rpcVmSnapshotList conn vmRef = do
  vClient <- getVmClient conn vmRef
  CGVm.Vm'snapshotList'results {CGVm.snapshots = ss} <-
    callOn #snapshotList CGVm.Vm'snapshotList'params vClient
  pure (map WVm.fromCapnpVmSnapshotInfo ss)

rpcVmSnapshotRollback :: CapnpConnection -> EntityRef -> Text -> IO ()
rpcVmSnapshotRollback conn vmRef name = do
  vClient <- getVmClient conn vmRef
  _ <-
    callOn
      #snapshotRollback
      CGVm.Vm'snapshotRollback'params {CGVm.name = name}
      vClient
  pure ()

rpcVmSnapshotDelete :: CapnpConnection -> EntityRef -> Text -> IO ()
rpcVmSnapshotDelete conn vmRef name = do
  vClient <- getVmClient conn vmRef
  _ <-
    callOn
      #snapshotDelete
      CGVm.Vm'snapshotDelete'params {CGVm.name = name}
      vClient
  pure ()

-- ---------------------------------------------------------------------
-- Shared directory
-- ---------------------------------------------------------------------

rpcSharedDirAdd
  :: CapnpConnection
  -> EntityRef
  -> Text
  -- ^ host path
  -> Text
  -- ^ tag
  -> SharedDirCache
  -> Bool
  -- ^ read-only
  -> IO Int64
rpcSharedDirAdd conn vmRef path tag cache readOnly = do
  vmClient <- getVmClient conn vmRef
  let p =
        CGVm.SharedDirAddParams
          { CGVm.path = path
          , CGVm.tag = tag
          , CGVm.cache = toCapnpSharedDirCache cache
          , CGVm.readOnly = readOnly
          }
  CGVm.Vm'addSharedDir'results {CGVm.sharedDirId = sid} <-
    callOn #addSharedDir CGVm.Vm'addSharedDir'params {CGVm.params = p} vmClient
  pure sid

rpcSharedDirRemove :: CapnpConnection -> EntityRef -> Int64 -> IO ()
rpcSharedDirRemove conn vmRef sharedDirId = do
  vmClient <- getVmClient conn vmRef
  _ <-
    callOn
      #removeSharedDir
      CGVm.Vm'removeSharedDir'params {CGVm.sharedDirId = sharedDirId}
      vmClient
  pure ()

rpcSharedDirList :: CapnpConnection -> EntityRef -> IO [PSd.SharedDirInfo]
rpcSharedDirList conn vmRef = do
  vmClient <- getVmClient conn vmRef
  CGVm.Vm'listSharedDirs'results {CGVm.sharedDirs = sds} <-
    callOn #listSharedDirs CGVm.Vm'listSharedDirs'params vmClient
  traverse (failOnWire . WSd.fromCapnpSharedDirInfo) sds

-- ---------------------------------------------------------------------
-- Network interface wrappers (per-VM)
-- ---------------------------------------------------------------------

rpcNetIfAdd
  :: CapnpConnection
  -> EntityRef
  -> NetInterfaceType
  -> Text
  -- ^ host device (\"\" → auto)
  -> Maybe Text
  -- ^ MAC pin (Nothing → daemon picks)
  -> Maybe EntityRef
  -- ^ managed network
  -> IO Int64
rpcNetIfAdd conn vmRef ifaceType hostDevice macAddress mNetwork = do
  vmClient <- getVmClient conn vmRef
  let p =
        CGVm.NetIfAddParams
          { CGVm.type_ = toCapnpNetInterfaceType ifaceType
          , CGVm.hostDevice = hostDevice
          , CGVm.macAddress = fromMaybe "" macAddress
          , CGVm.networkRef = maybe emptyCapnpEntityRef toCapnpEntityRef mNetwork
          }
  CGVm.Vm'addNetIf'results {CGVm.netIfId = nid} <-
    callOn #addNetIf CGVm.Vm'addNetIf'params {CGVm.params = p} vmClient
  pure nid

rpcNetIfRemove :: CapnpConnection -> EntityRef -> Int64 -> IO ()
rpcNetIfRemove conn vmRef netIfId = do
  vmClient <- getVmClient conn vmRef
  _ <-
    callOn
      #removeNetIf
      CGVm.Vm'removeNetIf'params {CGVm.netIfId = netIfId}
      vmClient
  pure ()

rpcNetIfList :: CapnpConnection -> EntityRef -> IO [PV.NetIfInfo]
rpcNetIfList conn vmRef = do
  vmClient <- getVmClient conn vmRef
  CGVm.Vm'listNetIfs'results {CGVm.netIfs = nis} <-
    callOn #listNetIfs CGVm.Vm'listNetIfs'params vmClient
  traverse (failOnWire . WVm.fromCapnpNetIfInfo) nis

-- ---------------------------------------------------------------------
-- SSH key attach / detach / list-for-vm
-- ---------------------------------------------------------------------

rpcSshKeyAttach :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcSshKeyAttach conn vmRef keyRef = do
  vmClient <- getVmClient conn vmRef
  _ <-
    callOn
      #attachSshKey
      CGVm.Vm'attachSshKey'params {CGVm.keyRef = toCapnpEntityRef keyRef}
      vmClient
  pure ()

rpcSshKeyDetach :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcSshKeyDetach conn vmRef keyRef = do
  vmClient <- getVmClient conn vmRef
  _ <-
    callOn
      #detachSshKey
      CGVm.Vm'detachSshKey'params {CGVm.keyRef = toCapnpEntityRef keyRef}
      vmClient
  pure ()

rpcSshKeyListForVm :: CapnpConnection -> EntityRef -> IO [PSk.SshKeyInfo]
rpcSshKeyListForVm conn vmRef = do
  vmClient <- getVmClient conn vmRef
  CGVm.Vm'listSshKeys'results {CGVm.keys = ks} <-
    callOn #listSshKeys CGVm.Vm'listSshKeys'params vmClient
  pure (map WSsh.fromCapnpSshKeyInfo ks)

-- ---------------------------------------------------------------------
-- VM streaming (Phase 6b)
-- ---------------------------------------------------------------------

data ClientByteSink = ClientByteSink
  { cbsOnWrite :: BS.ByteString -> IO ()
  , cbsOnEnd :: IO ()
  }

instance SomeServer ClientByteSink

instance CGS.ByteSink'server_ ClientByteSink where
  byteSink'write (ClientByteSink onWrite _) =
    handleParsed $ \CGS.ByteSink'write'params {CGS.chunk = chunk} -> do
      _ <- try (onWrite chunk) :: IO (Either SomeException ())
      pure CGS.ByteSink'write'results
  byteSink'end (ClientByteSink _ onEnd) =
    handleParsed $ \_ -> do
      _ <- try onEnd :: IO (Either SomeException ())
      pure CGS.ByteSink'end'results

streamByteSinkMethod
  :: ( C.IsCap iface
     , C.IsStruct params
     , C.IsStruct results
     , C.Parse params (C.Parsed params)
     , C.Parse results (C.Parsed results)
     )
  => CapnpConnection
  -> C.Client iface
  -> C.Method iface params results
  -> (C.Client CGS.ByteSink -> C.Parsed params)
  -> (C.Parsed results -> C.Client CGS.ByteSink)
  -> (BS.ByteString -> IO ())
  -> IO ()
  -> IO (BS.ByteString -> IO (), IO ())
streamByteSinkMethod conn iface method mkParams getInput onOut onEnd = do
  outSink <- export @CGS.ByteSink (ccSupervisor conn) (ClientByteSink onOut onEnd)
  results <- callOn method (mkParams outSink) iface
  let input = getInput results
      writeInput chunk =
        Control.Monad.void $
          callOn
            #write
            CGS.ByteSink'write'params {CGS.chunk = chunk}
            input
      endInput =
        Control.Monad.void (callOn #end CGS.ByteSink'end'params input)
  pure (writeInput, endInput)

rpcVmSerialConsole
  :: CapnpConnection
  -> EntityRef
  -> (BS.ByteString -> IO ())
  -> IO ()
  -> IO (BS.ByteString -> IO (), IO ())
rpcVmSerialConsole conn vmRef onOutput onEnd = do
  vmClient <- getVmClient conn vmRef
  streamByteSinkMethod
    conn
    vmClient
    #serialConsole
    (\sink -> CGVm.Vm'serialConsole'params {CGVm.sink = sink})
    (\CGVm.Vm'serialConsole'results {CGVm.input} -> input)
    onOutput
    onEnd

rpcVmHmpMonitor
  :: CapnpConnection
  -> EntityRef
  -> (BS.ByteString -> IO ())
  -> IO ()
  -> IO (BS.ByteString -> IO (), IO ())
rpcVmHmpMonitor conn vmRef onOutput onEnd = do
  vmClient <- getVmClient conn vmRef
  streamByteSinkMethod
    conn
    vmClient
    #hmpMonitor
    (\sink -> CGVm.Vm'hmpMonitor'params {CGVm.sink = sink})
    (\CGVm.Vm'hmpMonitor'results {CGVm.input} -> input)
    onOutput
    onEnd

-- ---------------------------------------------------------------------
-- Guest-agent subscription (Phase 6d)
-- ---------------------------------------------------------------------

data GuestAgentStatusEvent = GuestAgentStatusEvent
  { gaseVmId :: !Int64
  , gaseLastHealthcheck :: !Int64
  , gaseEnabled :: !Bool
  , gaseReachable :: !Bool
  , gaseMessage :: !Text
  }
  deriving (Eq, Show)

newtype ClientGuestAgentSink = ClientGuestAgentSink
  { cgasOnStatus :: GuestAgentStatusEvent -> IO ()
  }

instance SomeServer ClientGuestAgentSink

instance CGS.GuestAgentStatusSink'server_ ClientGuestAgentSink where
  guestAgentStatusSink'push (ClientGuestAgentSink onStatus) =
    handleParsed $ \CGS.GuestAgentStatusSink'push'params {CGS.status = s} -> do
      let CGS.GuestAgentStatus
            { CGS.vmId = vid
            , CGS.lastHealthcheck = lhc
            , CGS.enabled = en
            , CGS.reachable = rc
            } = s
          msg = case s of
            CGS.GuestAgentStatus {CGS.message = m} -> m
          ev =
            GuestAgentStatusEvent
              { gaseVmId = vid
              , gaseLastHealthcheck = lhc
              , gaseEnabled = en
              , gaseReachable = rc
              , gaseMessage = msg
              }
      _ <- try (onStatus ev) :: IO (Either SomeException ())
      pure CGS.GuestAgentStatusSink'push'results

rpcVmSubscribeGuestAgent
  :: CapnpConnection
  -> EntityRef
  -> (GuestAgentStatusEvent -> IO ())
  -> IO (C.Client CGS.Handle)
rpcVmSubscribeGuestAgent conn vmRef onStatus = do
  vmClient <- getVmClient conn vmRef
  sinkClient <-
    export @CGS.GuestAgentStatusSink
      (ccSupervisor conn)
      (ClientGuestAgentSink onStatus)
  CGVm.Vm'subscribeGuestAgent'results {CGVm.handle} <-
    callOn
      #subscribeGuestAgent
      CGVm.Vm'subscribeGuestAgent'params {CGVm.sink = sinkClient}
      vmClient
  pure handle

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

getVmClient :: CapnpConnection -> EntityRef -> IO (C.Client CGVm.Vm)
getVmClient conn ref = do
  CGCorvus.Daemon'vms'results {CGCorvus.mgr = mgr} <-
    callOn #vms CGCorvus.Daemon'vms'params (ccDaemon conn)
  CGVm.VmManager'get'results {CGVm.vm = vmClient} <-
    callOn #get CGVm.VmManager'get'params {CGVm.ref = toCapnpEntityRef ref} mgr
  pure vmClient

emptyCapnpEntityRef :: C.Parsed CGCommon.EntityRef
emptyCapnpEntityRef =
  CGCommon.EntityRef {CGCommon.union' = CGCommon.EntityRef'id 0}
