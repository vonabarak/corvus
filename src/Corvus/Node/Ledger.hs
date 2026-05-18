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

    -- * Spawned-process ledger
  , ProcessLedger
  , newProcessLedger
  , insertProcess
  , takeProcess
  , readProcess
  )
where

import Control.Concurrent.STM (STM, TVar, modifyTVar', newTVarIO, readTVar, stateTVar)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Word (Word32)
import System.Process (ProcessHandle)

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

-- ---------------------------------------------------------------------------
-- Spawned processes

-- | Map of PID → 'ProcessHandle' for every subprocess the agent
-- spawned this session. The handle is needed for @waitpid@-style
-- cleanup ('System.Process.waitForProcess' won't reap if you only
-- have the bare PID). Entries are inserted on spawn, removed on
-- stop or when the agent's internal waiter sees the process exit.
newtype ProcessLedger = ProcessLedger
  { processVar :: TVar (Map.Map Word32 ProcessHandle)
  }

newProcessLedger :: IO ProcessLedger
newProcessLedger = ProcessLedger <$> newTVarIO Map.empty

-- | Record a freshly-spawned process.
insertProcess :: ProcessLedger -> Word32 -> ProcessHandle -> STM ()
insertProcess l pid ph = modifyTVar' (processVar l) (Map.insert pid ph)

-- | Remove and return the handle for a PID, if any. Used by the
-- stop path before @waitForProcess@-ing the handle.
takeProcess :: ProcessLedger -> Word32 -> STM (Maybe ProcessHandle)
takeProcess l pid =
  stateTVar (processVar l) (\m -> (Map.lookup pid m, Map.delete pid m))

-- | Read but don't remove.
readProcess :: ProcessLedger -> Word32 -> STM (Maybe ProcessHandle)
readProcess l pid = Map.lookup pid <$> readTVar (processVar l)
