{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | VmManager + Vm cap implementations.
--
-- Phase 3 lands the read-only surface (list / show / status getters)
-- and wires the mutation methods to the existing Action machinery
-- where the conversion is straightforward. Methods not yet wired
-- fall through to 'methodUnimplemented'; the Cap'n Proto client
-- sees a clean @unimplemented@ exception until they ship.
module Corvus.Rpc.Vm
  ( VmManagerCap (..)
  , VmCap (..)
  , newVmManagerCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Vm as CGVm
import Capnp.Rpc (throwFailed, toClient)
import Capnp.Rpc.Server (SomeServer, handleParsed, methodUnimplemented)
import Corvus.Handlers.Resolve (resolveVm)
import Corvus.Handlers.Vm (handleVmList, handleVmShow)
import Corvus.Protocol (Response (..))
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Vm (toCapnpVmDetails, toCapnpVmInfo)
import Data.Int (Int64)
import Supervisors (Supervisor)

-- | VmManager capability — owns the supervisor used to export
-- per-VM resource caps.
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
    let vmCap = VmCap st sup eid
    client <- export @CGVm.Vm sup vmCap
    pure CGVm.VmManager'get'results {CGVm.vm = client}

  vmManager'create _ = methodUnimplemented

-- | Vm resource capability — wraps a resolved VM id plus the state
-- needed to drive subsequent operations on it.
data VmCap = VmCap
  { vmState :: !ServerState
  , _vmSup :: !Supervisor
  , vmId :: !Int64
  }

instance SomeServer VmCap

instance CGVm.Vm'server_ VmCap where
  vm'show (VmCap st _ eid) = handleParsed $ \_ -> do
    resp <- handleVmShow st eid
    case resp of
      RespVmDetails det ->
        -- Shared dirs aren't yet plumbed through 'handleVmShow';
        -- pass an empty list. Phase 6 will fold them in.
        pure CGVm.Vm'show'results {CGVm.details = toCapnpVmDetails det []}
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'show: unexpected response"

-- Other methods land in follow-up commits.
