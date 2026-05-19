-- | Per-process resource ledgers for `corvus-nodeagent`.
--
-- The agent is stateless across processes: nothing persists to
-- disk. These ledgers are the *in-memory* records of what the
-- current agent process instance has applied. Discarded on
-- shutdown; rebuilt by the daemon's on-(re)connect re-apply
-- loop.
module Corvus.Node.Ledger
  ( -- * VM ledger
    VmLedger
  , VmLiveState (..)
  , newVmLedger
  , readVms
  , lookupVm
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

import Control.Concurrent.STM (STM, TVar, modifyTVar', newTVarIO, readTVar, stateTVar)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Word (Word32)
import System.Process (ProcessHandle)

-- ---------------------------------------------------------------------------
-- VMs

-- | Per-VM runtime state the agent tracks once 'vmStart' has
-- successfully spawned QEMU. Indexed by vmId (the daemon-side
-- DB primary key).
data VmLiveState = VmLiveState
  { vlsQemuPid :: !Word32
  -- ^ The PID of the QEMU process.
  , vlsQemuHandle :: !ProcessHandle
  -- ^ The process handle for @waitForProcess@-style reaping.
  , vlsVirtiofsd :: ![(Word32, ProcessHandle)]
  -- ^ Per-shared-dir virtiofsd children. Empty when the VM had
  -- no shared dirs in its spec.
  , vlsLastExitCode :: !(TVar (Maybe Int))
  -- ^ Filled by the agent's internal reaper when QEMU exits.
  -- 'Nothing' while the VM is running; 'Just code' once the
  -- @waitForProcess@ call has returned. Readers ('vmStatus',
  -- 'vmStopGraceful') observe this to learn about exits without
  -- racing the reaper.
  , vlsStderrTail :: !(TVar T.Text)
  -- ^ Ring-buffered tail of QEMU's stderr (last ~4 KiB).
  -- Populated by a reader thread launched alongside QEMU; on
  -- early exit the wait-for-first-ping path quotes this back to
  -- the daemon so the error includes QEMU's own diagnostic
  -- output rather than a misleading "QGA ping timeout".
  , vlsSpicePort :: !Int32
  -- ^ Echoed back from the spec so 'vmStatus' can include it in
  -- 'VmRuntimeInfo' without a second lookup. 0 when no SPICE.
  }

-- | The agent-wide ledger of currently-tracked VMs, keyed by
-- vmId. Atomic STM access so the periodic poller, the RPC
-- handlers, and the reaper threads can interleave safely.
newtype VmLedger = VmLedger
  { vmVar :: TVar (Map.Map Int64 VmLiveState)
  }

newVmLedger :: IO VmLedger
newVmLedger = VmLedger <$> newTVarIO Map.empty

readVms :: VmLedger -> STM (Map.Map Int64 VmLiveState)
readVms = readTVar . vmVar

lookupVm :: VmLedger -> Int64 -> STM (Maybe VmLiveState)
lookupVm l vmId = Map.lookup vmId <$> readTVar (vmVar l)

insertVm :: VmLedger -> Int64 -> VmLiveState -> STM ()
insertVm l vmId st = modifyTVar' (vmVar l) (Map.insert vmId st)

removeVm :: VmLedger -> Int64 -> STM (Maybe VmLiveState)
removeVm l vmId =
  stateTVar (vmVar l) (\m -> (Map.lookup vmId m, Map.delete vmId m))

-- ---------------------------------------------------------------------------
-- Disks

-- | Key = disk-image logical name. Value = the spec the agent
-- has materialised on the filesystem; the underlying file is
-- the side effect. Polymorphic so the disk subsystem can pick
-- whatever record type it likes.
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
