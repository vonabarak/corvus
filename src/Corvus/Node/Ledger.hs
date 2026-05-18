-- | Per-process resource ledgers for `corvus-nodeagent`.
--
-- The agent is stateless across processes (Phase 1 onwards):
-- nothing persists to disk. These ledgers are the *in-memory*
-- records of what the agent's current process instance has
-- applied. Discarded on shutdown; rebuilt by the daemon's
-- on-(re)connect re-apply loop.
--
-- Polymorphic over the spec / live-state representations so
-- 'Corvus.Node.Disk' and 'Corvus.Node.Vm' (added in later
-- phases) can pick their own concrete types without dragging
-- this module into their import graphs.
--
-- Phase 1 ships only the type-and-constructor surface; no
-- caller populates the ledgers yet.
module Corvus.Node.Ledger
  ( -- * VM ledger
    VmLedger
  , newVmLedger
  , readVms
  , insertVm
  , removeVm

    -- * Disk ledger
  , DiskLedger
  , newDiskLedger
  , readDisks
  , insertDisk
  , removeDisk
  )
where

import Control.Concurrent.STM (STM, TVar, modifyTVar', newTVarIO, readTVar)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- VMs

-- | Key = VM name (e.g. @corvus-vm42@). Value = the spec last
-- applied + whatever live process/socket handles the state
-- machine allocated for it.
newtype VmLedger spec live = VmLedger
  { vmVar :: TVar (Map.Map T.Text (spec, live))
  }

newVmLedger :: IO (VmLedger spec live)
newVmLedger = VmLedger <$> newTVarIO Map.empty

readVms :: VmLedger spec live -> STM (Map.Map T.Text (spec, live))
readVms = readTVar . vmVar

insertVm
  :: VmLedger spec live -> T.Text -> spec -> live -> STM ()
insertVm l name spec live =
  modifyTVar' (vmVar l) (Map.insert name (spec, live))

removeVm :: VmLedger spec live -> T.Text -> STM ()
removeVm l name = modifyTVar' (vmVar l) (Map.delete name)

-- ---------------------------------------------------------------------------
-- Disks

-- | Key = disk-image logical name. Value = the spec the agent
-- has materialised on the filesystem; the underlying file is
-- the side effect.
newtype DiskLedger spec = DiskLedger
  { diskVar :: TVar (Map.Map T.Text spec)
  }

newDiskLedger :: IO (DiskLedger spec)
newDiskLedger = DiskLedger <$> newTVarIO Map.empty

readDisks :: DiskLedger spec -> STM (Map.Map T.Text spec)
readDisks = readTVar . diskVar

insertDisk :: DiskLedger spec -> T.Text -> spec -> STM ()
insertDisk l name spec = modifyTVar' (diskVar l) (Map.insert name spec)

removeDisk :: DiskLedger spec -> T.Text -> STM ()
removeDisk l name = modifyTVar' (diskVar l) (Map.delete name)
