{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Disk and VM snapshot RPCs.
module Corvus.NodeAgentClient.Snapshot
  ( snapshotCreate
  , snapshotDelete
  , snapshotRollback
  , snapshotCreateLive
  , snapshotCreateLiveMany
  , snapshotDeleteLive
  , snapshotCreateWithVmstate
  , snapshotLoadWithVmstate
  , snapshotDeleteWithVmstate
  , guestSetTime
  , QuiesceMode (..)
  )
where

import qualified Capnp.Gen.Enums as CGE
import qualified Capnp.Gen.Nodeagent as CGNA
import Corvus.NodeAgentClient.Core
import Corvus.NodeAgentClient.Disk (DiskOpResult, decodeDiskOpResult)
import Data.Int (Int64)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Snapshot operations

snapshotCreate
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> IO (Either NodeAgentError DiskOpResult)
snapshotCreate nac path name = remote $ do
  CGNA.Session'snapshotCreate'results {CGNA.result = r} <-
    callOn
      #snapshotCreate
      CGNA.Session'snapshotCreate'params {CGNA.path = path, CGNA.name = name}
      (nacSession nac)
  pure (decodeDiskOpResult r)

snapshotDelete
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> IO (Either NodeAgentError DiskOpResult)
snapshotDelete nac path name = remote $ do
  CGNA.Session'snapshotDelete'results {CGNA.result = r} <-
    callOn
      #snapshotDelete
      CGNA.Session'snapshotDelete'params {CGNA.path = path, CGNA.name = name}
      (nacSession nac)
  pure (decodeDiskOpResult r)

snapshotRollback
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> IO (Either NodeAgentError DiskOpResult)
snapshotRollback nac path name = remote $ do
  CGNA.Session'snapshotRollback'results {CGNA.result = r} <-
    callOn
      #snapshotRollback
      CGNA.Session'snapshotRollback'params {CGNA.path = path, CGNA.name = name}
      (nacSession nac)
  pure (decodeDiskOpResult r)

-- | Client-side mirror of the wire 'Capnp.Gen.Enums.QuiesceMode'.
-- Lives here so daemon-side callers don't import the generated
-- Cap'n Proto modules directly; the encoder in this module
-- translates.
data QuiesceMode
  = QuiesceAuto
  | QuiesceRequire
  | QuiesceSkip
  deriving (Eq, Show)

quiesceModeToWire :: QuiesceMode -> CGE.QuiesceMode
quiesceModeToWire QuiesceAuto = CGE.QuiesceMode'auto
quiesceModeToWire QuiesceRequire = CGE.QuiesceMode'require
quiesceModeToWire QuiesceSkip = CGE.QuiesceMode'skip

-- | Create a snapshot on a RUNNING VM via QMP. Returns the wire
-- 'DiskOpResult' plus a flag indicating whether the snapshot was
-- bracketed with QGA fsfreeze (i.e. whether it is genuinely
-- crash-consistent at the in-guest filesystem level).
snapshotCreateLive
  :: NodeAgentClient
  -> T.Text
  -- ^ qcow2 file path on the target node
  -> T.Text
  -- ^ snapshot name
  -> Int64
  -- ^ VM ID (the agent uses this to find the QMP socket)
  -> QuiesceMode
  -> IO (Either NodeAgentError (DiskOpResult, Bool))
snapshotCreateLive nac path name vmId qmode = remoteWithin 300 $ do
  CGNA.Session'snapshotCreateLive'results
    { CGNA.result = r
    , CGNA.quiesced = q
    } <-
    callOn
      #snapshotCreateLive
      CGNA.Session'snapshotCreateLive'params
        { CGNA.path = path
        , CGNA.name = name
        , CGNA.vmId = vmId
        , CGNA.quiesce = quiesceModeToWire qmode
        }
      (nacSession nac)
  pure (decodeDiskOpResult r, q)

-- | Atomic multi-disk live snapshot. Wraps N
-- @blockdev-snapshot-internal-sync@ actions in a single QMP
-- @transaction@; either every disk gets the named snapshot or
-- none of them do. QGA fsfreeze (per 'QuiesceMode') brackets the
-- whole transaction.
snapshotCreateLiveMany
  :: NodeAgentClient
  -> [T.Text]
  -- ^ qcow2 paths on the target node
  -> T.Text
  -- ^ snapshot name (same for every disk)
  -> Int64
  -- ^ VM ID
  -> QuiesceMode
  -> IO (Either NodeAgentError (DiskOpResult, Bool))
snapshotCreateLiveMany nac paths name vmId qmode = remoteWithin 300 $ do
  CGNA.Session'snapshotCreateLiveMany'results
    { CGNA.result = r
    , CGNA.quiesced = q
    } <-
    callOn
      #snapshotCreateLiveMany
      CGNA.Session'snapshotCreateLiveMany'params
        { CGNA.paths = paths
        , CGNA.name = name
        , CGNA.vmId = vmId
        , CGNA.quiesce = quiesceModeToWire qmode
        }
      (nacSession nac)
  pure (decodeDiskOpResult r, q)

-- | Delete a snapshot on a RUNNING VM via QMP. No QGA involvement
-- (snapshot deletion only touches qcow2 metadata, not guest I/O).
snapshotDeleteLive
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> Int64
  -> IO (Either NodeAgentError DiskOpResult)
snapshotDeleteLive nac path name vmId = remote $ do
  CGNA.Session'snapshotDeleteLive'results {CGNA.result = r} <-
    callOn
      #snapshotDeleteLive
      CGNA.Session'snapshotDeleteLive'params
        { CGNA.path = path
        , CGNA.name = name
        , CGNA.vmId = vmId
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

-- | Create a full-machine snapshot on a RUNNING VM via QMP
-- @snapshot-save@ async job. The vmstate (RAM + device + CPU
-- state) lands in the carrier disk's qcow2; sibling block
-- snapshots land in every disk in @devicePaths@ (which MUST
-- include the carrier). Requires QEMU >= 6.0; the agent
-- capability-probes and refuses with a clear error otherwise.
-- 5-minute timeout matches the agent-side poll budget.
snapshotCreateWithVmstate
  :: NodeAgentClient
  -> T.Text
  -- ^ vmstate carrier disk path
  -> [T.Text]
  -- ^ all writable disk paths to snapshot (must include carrier)
  -> T.Text
  -- ^ snapshot tag
  -> Int64
  -- ^ VM ID
  -> IO (Either NodeAgentError DiskOpResult)
snapshotCreateWithVmstate nac vmstatePath devicePaths name vmId =
  remoteWithin 360 $ do
    CGNA.Session'snapshotCreateWithVmstate'results {CGNA.result = r} <-
      callOn
        #snapshotCreateWithVmstate
        CGNA.Session'snapshotCreateWithVmstate'params
          { CGNA.vmstateDevicePath = vmstatePath
          , CGNA.devicePaths = devicePaths
          , CGNA.name = name
          , CGNA.vmId = vmId
          }
        (nacSession nac)
    pure (decodeDiskOpResult r)

-- | Restore a full-machine snapshot via QMP @snapshot-load@ async
-- job. The caller MUST ensure the VM's CPUs are paused (via
-- 'Corvus.Handlers.Vm' / direct QMP @stop@) before invoking;
-- @snapshot-load@ refuses to run with the CPUs live. The caller
-- is responsible for issuing QMP @cont@ after this returns and
-- the post-load setup (clock resync via 'guestSetTime', QGA
-- handshake) has completed.
snapshotLoadWithVmstate
  :: NodeAgentClient
  -> T.Text
  -- ^ vmstate carrier disk path
  -> [T.Text]
  -- ^ all disk paths that participated in the save
  -> T.Text
  -- ^ snapshot tag (must match save)
  -> Int64
  -- ^ VM ID
  -> IO (Either NodeAgentError DiskOpResult)
snapshotLoadWithVmstate nac vmstatePath devicePaths name vmId =
  remoteWithin 360 $ do
    CGNA.Session'snapshotLoadWithVmstate'results {CGNA.result = r} <-
      callOn
        #snapshotLoadWithVmstate
        CGNA.Session'snapshotLoadWithVmstate'params
          { CGNA.vmstateDevicePath = vmstatePath
          , CGNA.devicePaths = devicePaths
          , CGNA.name = name
          , CGNA.vmId = vmId
          }
        (nacSession nac)
    pure (decodeDiskOpResult r)

-- | Delete a full-machine snapshot via QMP @snapshot-delete@ async
-- job. Removes the vmstate AND the sibling block snapshots
-- atomically. Required for vmstate-aware snapshots because the
-- disk-only @blockdev-snapshot-delete-internal-sync@ leaves
-- vmstate orphaned in the carrier qcow2.
snapshotDeleteWithVmstate
  :: NodeAgentClient
  -> [T.Text]
  -- ^ all disk paths that participated in the snapshot
  -> T.Text
  -- ^ snapshot tag
  -> Int64
  -- ^ VM ID
  -> IO (Either NodeAgentError DiskOpResult)
snapshotDeleteWithVmstate nac devicePaths name vmId =
  remoteWithin 360 $ do
    CGNA.Session'snapshotDeleteWithVmstate'results {CGNA.result = r} <-
      callOn
        #snapshotDeleteWithVmstate
        CGNA.Session'snapshotDeleteWithVmstate'params
          { CGNA.devicePaths = devicePaths
          , CGNA.name = name
          , CGNA.vmId = vmId
          }
        (nacSession nac)
    pure (decodeDiskOpResult r)

-- | Tell QGA to resync the guest's wall clock from the host's
-- hardware clock. Used after a vmstate restore — the restored
-- guest thinks it's still snapshot-time, which breaks time-
-- sensitive operations. Best-effort: 'DiskOpResult' carries the
-- failure if QGA isn't reachable; the caller logs at WARN and
-- continues.
guestSetTime
  :: NodeAgentClient
  -> Int64
  -- ^ VM ID
  -> IO (Either NodeAgentError DiskOpResult)
guestSetTime nac vmId = remote $ do
  CGNA.Session'guestSetTime'results {CGNA.result = r} <-
    callOn
      #guestSetTime
      CGNA.Session'guestSetTime'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure (decodeDiskOpResult r)
