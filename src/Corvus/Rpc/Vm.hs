{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | VmManager + Vm cap implementations.
--
-- Phase 4 lands the non-streaming VM surface: create + the full
-- lifecycle (start/stop/pause/reset/edit/delete) plus the
-- shared-directory list for VmDetails. Streaming methods (serial
-- console, HMP monitor, guest-agent subscription) and the
-- sub-resource attach/detach/snapshot methods stay stubbed with
-- 'methodUnimplemented' until follow-up phases.
module Corvus.Rpc.Vm
  ( VmManagerCap (..)
  , VmCap (..)
  , newVmManagerCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Enums as CGE
import qualified Capnp.Gen.Vm as CGVm
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer, handleParsed, methodUnimplemented)
import Corvus.Action (runAction)
import Corvus.Handlers.Resolve (resolveVm)
import Corvus.Handlers.SharedDir (handleSharedDirList)
import Corvus.Handlers.Vm
  ( VmCreate (..)
  , VmDelete (..)
  , VmEdit (..)
  , VmPause (..)
  , VmReset (..)
  , VmStart (..)
  , VmStop (..)
  , handleVmList
  , handleVmShow
  )
import qualified Corvus.Model as M
import Corvus.Protocol (Response (..))
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Enums (toCapnpVmStatus)
import Corvus.Wire.Vm (toCapnpVmDetails, toCapnpVmInfo)
import Data.Int (Int64)
import qualified Data.Text as T
import Supervisors (Supervisor)

-- ---------------------------------------------------------------------
-- Manager
-- ---------------------------------------------------------------------

data VmManagerCap = VmManagerCap
  { vmgState :: !ServerState
  , vmgSup :: !Supervisor
  }

newVmManagerCap :: ServerState -> Supervisor -> IO VmManagerCap
newVmManagerCap st sup = pure (VmManagerCap st sup)

instance SomeServer VmManagerCap

instance CGVm.VmManager'server_ VmManagerCap where
  vmManager'list (VmManagerCap st _) = handleParsed $ \_ -> do
    resp <- handleVmList st
    case resp of
      RespVmList vms -> pure CGVm.VmManager'list'results {CGVm.vms = map toCapnpVmInfo vms}
      RespError msg -> throwFailed msg
      _ -> throwFailed "vmManager'list: unexpected response"

  vmManager'get (VmManagerCap st sup) = handleParsed $ \CGVm.VmManager'get'params {..} -> do
    ref' <- capnpRefToRef ref
    eid <- failOnLeft =<< resolveVm ref' (ssDbPool st)
    client <- export @CGVm.Vm sup (VmCap st sup eid)
    pure CGVm.VmManager'get'results {CGVm.vm = client}

  vmManager'create (VmManagerCap st sup) =
    handleParsed $ \CGVm.VmManager'create'params {params = CGVm.VmCreateParams {..}} -> do
      let act =
            VmCreate
              { vcrName = name
              , vcrCpuCount = fromIntegral cpuCount
              , vcrRamMb = fromIntegral ramMb
              , vcrDescription = if description == "" then Nothing else Just description
              , vcrHeadless = headless
              , vcrGuestAgent = guestAgent
              , vcrCloudInit = cloudInit
              , vcrAutostart = autostart
              }
      resp <- runAction st act
      case resp of
        RespVmCreated newId -> do
          client <- export @CGVm.Vm sup (VmCap st sup newId)
          pure CGVm.VmManager'create'results {CGVm.vm = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed (T.pack ("vmManager'create: unexpected response: " <> show resp))

-- ---------------------------------------------------------------------
-- Vm resource cap
-- ---------------------------------------------------------------------

data VmCap = VmCap
  { vmState :: !ServerState
  , _vmSup :: !Supervisor
  , vmId :: !Int64
  }

instance SomeServer VmCap

instance CGVm.Vm'server_ VmCap where
  vm'show (VmCap st _ eid) = handleParsed $ \_ -> do
    detResp <- handleVmShow st eid
    case detResp of
      RespVmDetails det -> do
        sharedResp <- handleSharedDirList st eid
        let sds = case sharedResp of
              RespSharedDirList xs -> xs
              _ -> []
        pure CGVm.Vm'show'results {CGVm.details = toCapnpVmDetails det sds}
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'show: unexpected response"

  vm'start (VmCap st _ eid) = handleParsed $ \CGVm.Vm'start'params {} -> do
    resp <- runAction st (VmStart eid)
    pure CGVm.Vm'start'results {CGVm.status = toStatusOrThrow resp}

  vm'stop (VmCap st _ eid) = handleParsed $ \CGVm.Vm'stop'params {} -> do
    resp <- runAction st (VmStop eid)
    pure CGVm.Vm'stop'results {CGVm.status = toStatusOrThrow resp}

  vm'pause (VmCap st _ eid) = handleParsed $ \_ -> do
    resp <- runAction st (VmPause eid)
    pure CGVm.Vm'pause'results {CGVm.status = toStatusOrThrow resp}

  vm'reset (VmCap st _ eid) = handleParsed $ \_ -> do
    resp <- runAction st (VmReset eid)
    pure CGVm.Vm'reset'results {CGVm.status = toStatusOrThrow resp}

  vm'edit (VmCap st _ eid) =
    handleParsed $ \CGVm.Vm'edit'params {params = CGVm.VmEditParams {..}} -> do
      let act =
            VmEdit
              { vedVmId = eid
              , vedCpus = if hasCpuCount then Just (fromIntegral cpuCount) else Nothing
              , vedRam = if hasRamMb then Just (fromIntegral ramMb) else Nothing
              , vedDesc = if hasDescription then Just description else Nothing
              , vedHeadless = if hasHeadless then Just headless else Nothing
              , vedGuestAgent = if hasGuestAgent then Just guestAgent else Nothing
              , vedCloudInit = if hasCloudInit then Just cloudInit else Nothing
              , vedAutostart = if hasAutostart then Just autostart else Nothing
              }
      resp <- runAction st act
      case resp of
        RespVmEdited -> pure CGVm.Vm'edit'results
        RespVmNotFound -> throwFailed "VM not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "vm'edit: unexpected response"

  vm'delete (VmCap st _ eid) = handleParsed $ \CGVm.Vm'delete'params {..} -> do
    resp <- runAction st (VmDelete {vdelVmId = eid, vdelDeleteDisks = deleteDisks})
    case resp of
      RespVmDeleted -> pure CGVm.Vm'delete'results
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'delete: unexpected response"

  -- Methods deferred to follow-up phases.
  vm'cloudInit _ = methodUnimplemented
  vm'viewGrant _ = methodUnimplemented
  vm'guestExec _ = methodUnimplemented
  vm'sendCtrlAltDel _ = methodUnimplemented
  vm'serialConsole _ = methodUnimplemented
  vm'serialConsoleFlush _ = methodUnimplemented
  vm'hmpMonitor _ = methodUnimplemented
  vm'hmpMonitorFlush _ = methodUnimplemented
  vm'subscribeGuestAgent _ = methodUnimplemented
  vm'attachDisk _ = methodUnimplemented
  vm'detachDisk _ = methodUnimplemented
  vm'addNetIf _ = methodUnimplemented
  vm'removeNetIf _ = methodUnimplemented
  vm'listNetIfs _ = methodUnimplemented
  vm'addSharedDir _ = methodUnimplemented
  vm'removeSharedDir _ = methodUnimplemented
  vm'listSharedDirs _ = methodUnimplemented
  vm'snapshotCreate _ = methodUnimplemented
  vm'snapshotList _ = methodUnimplemented
  vm'snapshotGet _ = methodUnimplemented
  vm'attachSshKey _ = methodUnimplemented
  vm'detachSshKey _ = methodUnimplemented
  vm'listSshKeys _ = methodUnimplemented

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

toStatusOrThrow :: Response -> CGE.VmStatus
toStatusOrThrow resp = case resp of
  RespVmStateChanged s -> toCapnpVmStatus s
  RespVmRunning -> toCapnpVmStatus M.VmRunning
  _ -> CGE.VmStatus'error
