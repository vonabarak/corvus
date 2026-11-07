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
  , stopVmWorkers
  , vmOperationLocks
  , newVmLedger
  , readVms
  , lookupVm
  , removeVm
  , removeVmIfCurrent
  , VmStartAdmission (..)
  , admitVmStart
  , publishVmStart
  , clearVmStartReservation
  , waitForVmStarts
  , fenceVmStop
  , fenceVmReset
  )
where

import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import qualified Corvus.Node.VmSpec as VS
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
  -- ^ The PID of the QEMU process. Mutated in place by the
  -- reboot-quirk re-spawn path; readers expecting a stable PID
  -- across an entry's lifetime should not assume identity.
  , vlsQemuHandle :: !ProcessHandle
  -- ^ The process handle for @waitForProcess@-style reaping.
  -- Also mutated in place on reboot-quirk re-spawn.
  , vlsVirtiofsd :: ![(Word32, ProcessHandle)]
  -- ^ Per-shared-dir virtiofsd children. Empty when the VM had
  -- no shared dirs in its spec.
  , vlsSwtpm :: !(Maybe (Word32, ProcessHandle))
  -- ^ The per-VM swtpm child when TPM 2.0 is enabled.
  , vlsLastExitCode :: !(TVar (Maybe Int))
  -- ^ Filled by the agent's internal reaper when QEMU exits.
  -- 'Nothing' while the VM is running; 'Just code' once the
  -- @waitForProcess@ call has returned. Readers ('vmStatus',
  -- 'vmStopGraceful') observe this to learn about exits without
  -- racing the reaper. The reboot-quirk re-spawn path
  -- deliberately keeps this 'Nothing' across the QEMU bounce —
  -- the daemon's monitor must not see a transient stopped state.
  , vlsStderrTail :: !(TVar T.Text)
  -- ^ Ring-buffered tail of QEMU's stderr (last ~4 KiB).
  -- Populated by a reader thread launched alongside QEMU; on
  -- early exit the wait-for-first-ping path quotes this back to
  -- the daemon so the error includes QEMU's own diagnostic
  -- output rather than a misleading "QGA ping timeout".
  , vlsStderrDone :: !(TVar Bool)
  -- ^ Set once the stderr reader finishes, so startup failures can include
  -- diagnostics still in the pipe when the process exits.
  , vlsStartupError :: !(TVar (Maybe T.Text))
  -- ^ First guest-agent ping timed out while QEMU remained alive.
  -- Retained in every status snapshot so reconnects cannot promote
  -- this runtime back to running.
  , vlsStartupPending :: !(TVar Bool)
  -- ^ The startup coordinator owns QGA until its first-ping wait finishes.
  -- Short periodic health probes must not connect first and cancel that
  -- handshake while the guest is still booting.
  , vlsSpicePort :: !Int32
  -- ^ Echoed back from the spec so 'vmStatus' can include it in
  -- 'VmRuntimeInfo' without a second lookup. 0 when no SPICE.
  , vlsSpec :: !VS.VmSpec
  -- ^ The 'VmSpec' the daemon sent on 'vmStart'. Kept so the
  -- reboot-quirk re-spawn path can re-launch QEMU with identical
  -- settings (drives, NICs, shared dirs, vsock CID, …) without
  -- having to round-trip back to the daemon.
  , vlsWorkers :: !(TVar [Async ()])
  , vlsStopRequested :: !(TVar Bool)
  -- ^ Set by 'handleVmStopGraceful' / 'handleVmStopHard' before
  -- they signal QEMU. The reaper consults this flag on QEMU
  -- exit: when 'True' the exit is daemon-initiated and the
  -- reaper records 'vlsLastExitCode' normally; when 'False'
  -- (and 'vsRebootQuirk' is on) the exit is guest-initiated
  -- and the reaper re-spawns QEMU instead.
  }

-- | The agent-wide ledger of currently-tracked VMs, keyed by
-- vmId. Atomic STM access so the periodic poller, the RPC
-- handlers, and the reaper threads can interleave safely.
data VmFence = VmFence
  { vfHighestRevision :: !Int64
  , vfStartReservation :: !(Maybe Int64)
  }

-- | Process-wide live-state ledger plus retained per-VM fence tombstones.
-- Tombstones deliberately survive removal of a live entry: a delayed start
-- RPC must not become valid merely because reset already removed QEMU.
data VmLedger = VmLedger
  { vmVar :: TVar (Map.Map Int64 VmLiveState)
  , vmOperationLocks :: TVar (Map.Map Int64 (MVar ()))
  , vmStarts :: TVar (Map.Map Int64 (Int64, Int64))
  , vmFences :: TVar (Map.Map Int64 VmFence)
  }

data VmStartAdmission
  = VmStartAccepted (Maybe VmLiveState)
  | VmStartAlreadyRunning VmLiveState
  | VmStartRejected T.Text

newVmLedger :: IO VmLedger
newVmLedger = VmLedger <$> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO Map.empty

readVms :: VmLedger -> STM (Map.Map Int64 VmLiveState)
readVms = readTVar . vmVar

lookupVm :: VmLedger -> Int64 -> STM (Maybe VmLiveState)
lookupVm l vmId = Map.lookup vmId <$> readTVar (vmVar l)

removeVm :: VmLedger -> Int64 -> STM (Maybe VmLiveState)
removeVm l vmId =
  stateTVar (vmVar l) (\m -> (Map.lookup vmId m, Map.delete vmId m))

-- | Remove a live entry only when it still belongs to the start operation
-- performing cleanup. A reset or newer start may have replaced it meanwhile.
removeVmIfCurrent :: VmLedger -> Int64 -> Int64 -> Int64 -> STM (Maybe VmLiveState)
removeVmIfCurrent ledger vmId revision generation = do
  mLive <- lookupVm ledger vmId
  case mLive of
    Just live
      | VS.vsLifecycleRevision (vlsSpec live) == revision
          && VS.vsRuntimeGeneration (vlsSpec live) == generation ->
          removeVm ledger vmId
    _ -> pure Nothing

-- | Reserve a cold start under its daemon-owned fence. The reservation is
-- created before helpers/QEMU spawn and is invalidated atomically by reset.
admitVmStart :: VmLedger -> Int64 -> Int64 -> Int64 -> STM VmStartAdmission
admitVmStart ledger vmId revision generation = do
  live <- lookupVm ledger vmId
  fences <- readTVar (vmFences ledger)
  let fence = Map.findWithDefault (VmFence (-1) Nothing) vmId fences
      reserveStart
        | revision <= vfHighestRevision fence =
            pure (VmStartRejected "stale lifecycle revision")
        | otherwise = do
            writeTVar
              (vmFences ledger)
              (Map.insert vmId (VmFence revision (Just generation)) fences)
            modifyTVar' (vmStarts ledger) (Map.insert vmId (revision, generation))
            pure (VmStartAccepted Nothing)
  case live of
    Just current -> do
      mExit <- readTVar (vlsLastExitCode current)
      case mExit of
        Nothing
          | VS.vsLifecycleRevision (vlsSpec current) == revision
              && VS.vsRuntimeGeneration (vlsSpec current) == generation ->
              pure (VmStartAlreadyRunning current)
          | otherwise -> pure (VmStartRejected "a runtime is already live")
        Just _ -> do
          stopping <- readTVar (vlsStopRequested current)
          if stopping
            then pure (VmStartRejected "runtime cleanup is still pending")
            else do
              _ <- removeVm ledger vmId
              admission <- reserveStart
              pure $ case admission of
                VmStartAccepted Nothing -> VmStartAccepted (Just current)
                other -> other
    Nothing -> reserveStart

-- | Publish a spawned process only while the reservation remains current.
-- A reset that raced any part of start changes the high-water mark or clears
-- the reservation, making this return False; the caller must tear down the
-- just-spawned process.
publishVmStart :: VmLedger -> Int64 -> Int64 -> Int64 -> VmLiveState -> STM Bool
publishVmStart ledger vmId revision generation live = do
  fences <- readTVar (vmFences ledger)
  let fence = Map.findWithDefault (VmFence (-1) Nothing) vmId fences
      current = vfHighestRevision fence == revision
      reserved = vfStartReservation fence == Just generation
  existing <- lookupVm ledger vmId
  respawning <- case existing of
    Just old -> do
      stopped <- readTVar (vlsStopRequested old)
      pure $
        not stopped
          && VS.vsLifecycleRevision (vlsSpec old) == revision
          && VS.vsRuntimeGeneration (vlsSpec old) == generation
    Nothing -> pure False
  if current && (reserved || respawning)
    then do
      modifyTVar' (vmVar ledger) (Map.insert vmId live)
      writeTVar (vmFences ledger) (Map.insert vmId (VmFence revision Nothing) fences)
      pure True
    else do
      -- Even a fenced spawn remains owned until its caller confirms cleanup.
      -- The VM operation lock prevents a newer spawn from occupying this slot.
      writeTVar (vlsStopRequested live) True
      modifyTVar' (vmVar ledger) (Map.insertWith (\new old -> if VS.vsRuntimeGeneration (vlsSpec old) == generation then new else old) vmId live)
      pure False

clearVmStartReservation :: VmLedger -> Int64 -> Int64 -> Int64 -> STM ()
clearVmStartReservation ledger vmId revision generation = do
  modifyTVar' (vmStarts ledger) (Map.update (\token -> if token == (revision, generation) then Nothing else Just token) vmId)
  fences <- readTVar (vmFences ledger)
  case Map.lookup vmId fences of
    Just fence
      | vfHighestRevision fence == revision
          && vfStartReservation fence == Just generation ->
          writeTVar (vmFences ledger) (Map.insert vmId (VmFence revision Nothing) fences)
    _ -> pure ()

-- | Advance the reset fence and retain the current runtime until cleanup. A
-- revision lower than a previously accepted command is stale and cannot kill
-- a newer runtime.
fenceVmReset :: VmLedger -> Int64 -> Int64 -> STM (Either T.Text (Maybe VmLiveState))
fenceVmReset ledger vmId revision = do
  fences <- readTVar (vmFences ledger)
  let fence = Map.findWithDefault (VmFence (-1) Nothing) vmId fences
  if revision < vfHighestRevision fence
    then pure (Left "stale lifecycle revision")
    else do
      writeTVar (vmFences ledger) (Map.insert vmId (VmFence revision Nothing) fences)
      live <- lookupVm ledger vmId
      case live of
        Nothing -> pure ()
        Just current -> writeTVar (vlsStopRequested current) True
      pure (Right live)

-- | Wait for invalidated starts to finish disposing of unpublished children.
waitForVmStarts :: VmLedger -> Int64 -> STM ()
waitForVmStarts ledger vmId = readTVar (vmStarts ledger) >>= check . Map.notMember vmId

-- | Stop without changing the daemon revision, invalidating pending publication.
fenceVmStop :: VmLedger -> Int64 -> STM (Either T.Text (Maybe VmLiveState))
fenceVmStop ledger vmId = do
  fences <- readTVar (vmFences ledger)
  fenceVmReset ledger vmId (maybe (-1) vfHighestRevision (Map.lookup vmId fences))

-- | Called after confirmed QEMU exit and on agent shutdown. Cancellation joins
-- each worker, so no retired runtime can publish another socket handle.
stopVmWorkers :: VmLiveState -> IO ()
stopVmWorkers live = do
  workers <- readTVarIO (vlsWorkers live)
  mapM_ cancel workers
  atomically $ modifyTVar' (vlsWorkers live) (filter (`notElem` workers))
