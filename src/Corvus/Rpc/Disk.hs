{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | DiskManager + Disk + Snapshot cap implementations.
module Corvus.Rpc.Disk
  ( DiskManagerCap (..)
  , DiskCap (..)
  , SnapshotCap (..)
  , newDiskManagerCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Disk as CGDisk
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer, handleParsed, methodUnimplemented)
import Corvus.Handlers.Disk (handleDiskList, handleDiskShow)
import Corvus.Handlers.Resolve (resolveDisk)
import Corvus.Protocol (Response (..))
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Disk (toCapnpDiskImageInfo)
import Data.Int (Int64)
import Supervisors (Supervisor)

data DiskManagerCap = DiskManagerCap
  { dmState :: !ServerState
  , dmSup :: !Supervisor
  }

newDiskManagerCap :: ServerState -> Supervisor -> IO DiskManagerCap
newDiskManagerCap st sup = pure (DiskManagerCap st sup)

instance SomeServer DiskManagerCap

instance CGDisk.DiskManager'server_ DiskManagerCap where
  diskManager'list (DiskManagerCap st _) = handleParsed $ \_ -> do
    resp <- handleDiskList st
    case resp of
      RespDiskList disks ->
        pure CGDisk.DiskManager'list'results {CGDisk.disks = map toCapnpDiskImageInfo disks}
      RespError msg -> throwFailed msg
      _ -> throwFailed "diskManager'list: unexpected response"

  diskManager'get (DiskManagerCap st sup) = handleParsed $ \CGDisk.DiskManager'get'params {..} -> do
    ref' <- capnpRefToRef ref
    eid <- failOnLeft =<< resolveDisk ref' (ssDbPool st)
    client <- export @CGDisk.Disk sup (DiskCap st sup eid)
    pure CGDisk.DiskManager'get'results {CGDisk.disk = client}

  diskManager'create _ = methodUnimplemented
  diskManager'register _ = methodUnimplemented
  diskManager'createOverlay _ = methodUnimplemented
  diskManager'clone _ = methodUnimplemented
  diskManager'rebase _ = methodUnimplemented
  diskManager'importUrl _ = methodUnimplemented
  diskManager'import_ _ = methodUnimplemented

data DiskCap = DiskCap
  { _dskState :: !ServerState
  , _dskSup :: !Supervisor
  , _dskId :: !Int64
  }

instance SomeServer DiskCap

instance CGDisk.Disk'server_ DiskCap where
  disk'show (DiskCap st _ eid) = handleParsed $ \_ -> do
    resp <- handleDiskShow st eid
    case resp of
      RespDiskInfo info ->
        pure CGDisk.Disk'show'results {CGDisk.info = toCapnpDiskImageInfo info}
      RespDiskNotFound -> throwFailed "Disk not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "disk'show: unexpected response"

  disk'delete _ = methodUnimplemented
  disk'refresh _ = methodUnimplemented
  disk'resize _ = methodUnimplemented
  disk'snapshotCreate _ = methodUnimplemented
  disk'snapshotList _ = methodUnimplemented
  disk'snapshotGet _ = methodUnimplemented

data SnapshotCap = SnapshotCap
  { _snState :: !ServerState
  , _snId :: !Int64
  }

instance SomeServer SnapshotCap

instance CGDisk.Snapshot'server_ SnapshotCap where
  snapshot'show _ = methodUnimplemented
  snapshot'delete _ = methodUnimplemented
  snapshot'rollback _ = methodUnimplemented
  snapshot'merge _ = methodUnimplemented
