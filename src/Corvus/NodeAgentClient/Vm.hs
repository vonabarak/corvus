{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VM lifecycle, console, and QMP-mediated runtime RPCs.
module Corvus.NodeAgentClient.Vm
  ( vmStart
  , VmStartOutcome (..)
  , vmStopGraceful
  , vmStopHard
  , vmPause
  , vmResume
  , vmSave
  , deleteSavedState
  , deleteTpmState
  , vmGuestExec
  , vmGuestExecStream
  , vmStatus
  , vmSetSpiceTicket
  , subscribeVmStatus
  , openSerialConsole
  , openHmpMonitor
  , flushSerialConsole
  , flushHmpMonitor
  , vmAttachDrive
  , vmDetachDrive
  , vmEjectMedia
  , vmChangeMedia
  , probeVsockCid
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Streams as CGS
import Corvus.Node.VmSpec
  ( VmAgentState (..)
  , VmAgentStatus (..)
  , VmDriveSpec (..)
  , VmGuestExecInfo (..)
  , VmGuestExecReq (..)
  , VmNetIfSpec (..)
  , VmRuntimeInfo (..)
  , VmSharedDirSpec (..)
  , VmSpec (..)
  , VmStopKind (..)
  , VmStopResult (..)
  )
import Corvus.NodeAgentClient.Core
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Data.Word (Word32)

data VmStartOutcome
  = VmStartStarted VmRuntimeInfo
  | VmStartVsockCidBusy
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Encoders / decoders for VM abstraction wire types.

encodeVmSpec :: VmSpec -> CGNA.Parsed CGNA.VmSpec
encodeVmSpec s =
  CGNA.VmSpec
    { CGNA.vmId = vsVmId s
    , CGNA.lifecycleRevision = vsLifecycleRevision s
    , CGNA.runtimeGeneration = vsRuntimeGeneration s
    , CGNA.name = vsName s
    , CGNA.cpuCount = vsCpuCount s
    , CGNA.ramMb = vsRamMb s
    , CGNA.headless = vsHeadless s
    , CGNA.guestAgent = vsGuestAgent s
    , CGNA.tpm = vsTpm s
    , CGNA.vsockCid = fromMaybe 0 (vsVsockCid s)
    , CGNA.hasVsockCid = isJust (vsVsockCid s)
    , CGNA.spicePort = fromMaybe 0 (vsSpicePort s)
    , CGNA.hasSpicePort = isJust (vsSpicePort s)
    , CGNA.drives = map encodeVmDriveSpec (vsDrives s)
    , CGNA.netIfs = map encodeVmNetIfSpec (vsNetIfs s)
    , CGNA.sharedDirs = map encodeVmSharedDirSpec (vsSharedDirs s)
    , CGNA.waitForGuestAgentMs = vsWaitForGuestAgentMs s
    , CGNA.rebootQuirk = vsRebootQuirk s
    , CGNA.spiceBindAddr = vsSpiceBindAddr s
    , CGNA.loadFromSavedState = vsLoadFromSavedState s
    , CGNA.cpuModel = vsCpuModel s
    , CGNA.startPaused = vsStartPaused s
    }

encodeVmDriveSpec :: VmDriveSpec -> CGNA.Parsed CGNA.VmDriveSpec
encodeVmDriveSpec d =
  CGNA.VmDriveSpec
    { CGNA.driveId = vdsDriveId d
    , -- \^ a drive with no media (ejected CD-ROM tray) encodes as an
      -- empty wire path
      CGNA.diskFilePath = fromMaybe "" (vdsDiskFilePath d)
    , CGNA.format = vdsFormat d
    , CGNA.ifKind = vdsIfKind d
    , CGNA.media = vdsMedia d
    , CGNA.readOnly = vdsReadOnly d
    , CGNA.cache = vdsCache d
    , CGNA.discard = vdsDiscard d
    }

encodeVmNetIfSpec :: VmNetIfSpec -> CGNA.Parsed CGNA.VmNetIfSpec
encodeVmNetIfSpec n =
  CGNA.VmNetIfSpec
    { CGNA.ifType = vnsIfType n
    , CGNA.hostDevice = vnsHostDevice n
    , CGNA.macAddress = vnsMacAddress n
    }

encodeVmSharedDirSpec :: VmSharedDirSpec -> CGNA.Parsed CGNA.VmSharedDirSpec
encodeVmSharedDirSpec s =
  CGNA.VmSharedDirSpec
    { CGNA.hostPath = vssHostPath s
    , CGNA.tag = vssTag s
    , CGNA.cache = vssCache s
    , CGNA.readOnly = vssReadOnly s
    }

decodeVmRuntimeInfo :: CGNA.Parsed CGNA.VmRuntimeInfo -> VmRuntimeInfo
decodeVmRuntimeInfo
  CGNA.VmRuntimeInfo
    { CGNA.qemuPid = q
    , CGNA.virtiofsdPids = vs
    , CGNA.spicePort = sp
    , CGNA.swtpmPid = tp
    , CGNA.lifecycleRevision = rev
    , CGNA.runtimeGeneration = gen
    } =
    VmRuntimeInfo
      { vriQemuPid = q
      , vriVirtiofsdPids = vs
      , vriSpicePort = sp
      , vriSwtpmPid = tp
      , vriLifecycleRevision = rev
      , vriRuntimeGeneration = gen
      }

decodeVmStopResult :: CGNA.Parsed CGNA.VmStopResult -> VmStopResult
decodeVmStopResult CGNA.VmStopResult {CGNA.kind = k, CGNA.message = m} =
  VmStopResult
    { vsrKind = case k of
        CGNA.VmStopKind'stopped -> VmStopStopped
        CGNA.VmStopKind'alreadyStopped -> VmStopAlreadyStopped
        CGNA.VmStopKind'timeout -> VmStopTimeout
        CGNA.VmStopKind'failed -> VmStopFailed
        CGNA.VmStopKind'unknown' _ -> VmStopFailed
    , vsrMessage = m
    }

decodeVmAgentStatus :: CGNA.Parsed CGNA.VmAgentStatus -> VmAgentStatus
decodeVmAgentStatus
  CGNA.VmAgentStatus
    { CGNA.state = s
    , CGNA.qemuPid = q
    , CGNA.lastExitCode = e
    } =
    VmAgentStatus
      { vasState = case s of
          CGNA.VmAgentState'running -> VmAgentRunning
          CGNA.VmAgentState'stopped -> VmAgentStopped
          CGNA.VmAgentState'errored -> VmAgentErrored
          CGNA.VmAgentState'unknown -> VmAgentUnknown
          CGNA.VmAgentState'unknown' _ -> VmAgentUnknown
      , vasQemuPid = q
      , vasLastExitCode = e
      }

encodeVmGuestExecReq :: VmGuestExecReq -> CGNA.Parsed CGNA.VmGuestExecReq
encodeVmGuestExecReq r =
  CGNA.VmGuestExecReq
    { CGNA.vmId = vgeVmId r
    , CGNA.path = vgePath r
    , CGNA.args = vgeArgs r
    , CGNA.captureOutput = vgeCaptureOutput r
    , CGNA.inputData = vgeInputData r
    , CGNA.timeoutSec = vgeTimeoutSec r
    }

decodeVmGuestExecInfo :: CGNA.Parsed CGNA.VmGuestExecInfo -> VmGuestExecInfo
decodeVmGuestExecInfo
  CGNA.VmGuestExecInfo
    { CGNA.exitCode = c
    , CGNA.hasExit = h
    , CGNA.signal = sg
    , CGNA.stdout = so
    , CGNA.stderr = se
    } =
    VmGuestExecInfo
      { vgiExitCode = c
      , vgiHasExit = h
      , vgiSignal = sg
      , vgiStdout = so
      , vgiStderr = se
      }

-- ---------------------------------------------------------------------------
-- VM abstraction — client wrappers.

vmStart :: NodeAgentClient -> VmSpec -> IO (Either NodeAgentError VmStartOutcome)
vmStart nac spec = remoteWithin 120 $ do
  CGNA.Session'vmStart'results {CGNA.result = result} <-
    callOn
      #vmStart
      CGNA.Session'vmStart'params {CGNA.spec = encodeVmSpec spec}
      (nacSession nac)
  pure $ case CGNA.union' result of
    CGNA.VmStartResult'started i -> VmStartStarted (decodeVmRuntimeInfo i)
    CGNA.VmStartResult'vsockCidBusy -> VmStartVsockCidBusy
    CGNA.VmStartResult'unknown' tag -> error ("unknown VmStartResult tag: " <> show tag)

vmStopGraceful
  :: NodeAgentClient
  -> Int64
  -> Word32
  -> IO (Either NodeAgentError VmStopResult)
vmStopGraceful nac vmId timeoutSec = remoteWithin (fromIntegral timeoutSec + 30) $ do
  CGNA.Session'vmStopGraceful'results {CGNA.result = r} <-
    callOn
      #vmStopGraceful
      CGNA.Session'vmStopGraceful'params
        { CGNA.vmId = vmId
        , CGNA.timeoutSec = timeoutSec
        }
      (nacSession nac)
  pure (decodeVmStopResult r)

vmStopHard
  :: NodeAgentClient
  -> Int64
  -> Maybe Int64
  -> IO (Either NodeAgentError VmStopResult)
vmStopHard nac vmId mFence = remote $ do
  CGNA.Session'vmStopHard'results {CGNA.result = r} <-
    callOn
      #vmStopHard
      CGNA.Session'vmStopHard'params
        { CGNA.vmId = vmId
        , CGNA.lifecycleRevision = fromMaybe 0 mFence
        , CGNA.hasLifecycleFence = isJust mFence
        }
      (nacSession nac)
  pure (decodeVmStopResult r)

vmPause :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError ())
vmPause nac vmId = remote $ do
  _ :: C.Parsed CGNA.Session'vmPause'results <-
    callOn
      #vmPause
      CGNA.Session'vmPause'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure ()

vmResume :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError ())
vmResume nac vmId = remote $ do
  _ :: C.Parsed CGNA.Session'vmResume'results <-
    callOn
      #vmResume
      CGNA.Session'vmResume'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure ()

-- | Ask the agent to save the VM's running state to disk
-- (QMP @migrate file:…@), wait for completion, then terminate
-- QEMU. The agent owns the path convention; the daemon only
-- needs to know which VM to save. Throws on migration failure.
vmSave :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError ())
vmSave nac vmId = remoteWithin 330 $ do
  _ :: C.Parsed CGNA.Session'vmSave'results <-
    callOn
      #vmSave
      CGNA.Session'vmSave'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure ()

-- | Ask the agent to unlink the per-VM saved-state file. Used by
-- the daemon's reset-from-saved and delete-saved paths.
-- Idempotent on the agent — a missing file is success.
deleteSavedState :: NodeAgentClient -> T.Text -> IO (Either NodeAgentError ())
deleteSavedState nac vmName = remote $ do
  _ :: C.Parsed CGNA.Session'deleteSavedState'results <-
    callOn
      #deleteSavedState
      CGNA.Session'deleteSavedState'params {CGNA.vmName = vmName}
      (nacSession nac)
  pure ()

-- | Ask the agent to remove persistent swtpm state for a VM.
-- Idempotent on the agent; a missing directory is success.
deleteTpmState :: NodeAgentClient -> T.Text -> IO (Either NodeAgentError ())
deleteTpmState nac vmName = remote $ do
  _ :: C.Parsed CGNA.Session'deleteTpmState'results <-
    callOn
      #deleteTpmState
      CGNA.Session'deleteTpmState'params {CGNA.vmName = vmName}
      (nacSession nac)
  pure ()

vmGuestExec
  :: NodeAgentClient -> VmGuestExecReq -> IO (Either NodeAgentError VmGuestExecInfo)
vmGuestExec nac r = remote $ do
  CGNA.Session'vmGuestExec'results {CGNA.info = i} <-
    callOn
      #vmGuestExec
      CGNA.Session'vmGuestExec'params {CGNA.req = encodeVmGuestExecReq r}
      (nacSession nac)
  pure (decodeVmGuestExecInfo i)

-- | Streaming variant of 'vmGuestExec'. The agent pushes
-- incremental stdout / stderr bytes through the caller-supplied
-- sinks while the guest process runs; the returned
-- 'VmGuestExecInfo' carries only the exit code. The caller MUST
-- export the sinks BEFORE this call (they must be live for the
-- duration of the exec) and end-of-stream signalling arrives via
-- the agent calling @sink.end()@ on completion.
vmGuestExecStream
  :: NodeAgentClient
  -> VmGuestExecReq
  -> C.Client CGS.ByteSink
  -- ^ stdout sink (exported by the caller)
  -> C.Client CGS.ByteSink
  -- ^ stderr sink (exported by the caller)
  -> IO (Either NodeAgentError VmGuestExecInfo)
vmGuestExecStream nac r stdoutSink stderrSink = remote $ do
  CGNA.Session'vmGuestExecStream'results {CGNA.info = i} <-
    callOn
      #vmGuestExecStream
      CGNA.Session'vmGuestExecStream'params
        { CGNA.req = encodeVmGuestExecReq r
        , CGNA.stdoutSink = stdoutSink
        , CGNA.stderrSink = stderrSink
        }
      (nacSession nac)
  pure (decodeVmGuestExecInfo i)

vmStatus :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError VmAgentStatus)
vmStatus nac vmId = remote $ do
  CGNA.Session'vmStatus'results {CGNA.status = s} <-
    callOn
      #vmStatus
      CGNA.Session'vmStatus'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure (decodeVmAgentStatus s)

vmSetSpiceTicket
  :: NodeAgentClient -> Int64 -> T.Text -> Word32 -> IO (Either NodeAgentError ())
vmSetSpiceTicket nac vmId password ttlSeconds = remote $ do
  _ :: C.Parsed CGNA.Session'vmSetSpiceTicket'results <-
    callOn
      #vmSetSpiceTicket
      CGNA.Session'vmSetSpiceTicket'params
        { CGNA.vmId = vmId
        , CGNA.password = password
        , CGNA.ttlSeconds = ttlSeconds
        }
      (nacSession nac)
  pure ()

-- | Register a 'VmStatusSink' with the agent. The agent retains
-- a reference and pushes a 'VmStatusSnapshot' to it every ~10 s
-- until the sink throws (at which point it is pruned). One
-- registration per agent connection is sufficient — the daemon
-- calls this once from its on-connect callback.
subscribeVmStatus
  :: NodeAgentClient
  -> C.Client CGNA.VmStatusSink
  -> IO (Either NodeAgentError ())
subscribeVmStatus nac sink = remote $ do
  _ :: C.Parsed CGNA.Session'subscribeVmStatus'results <-
    callOn
      #subscribeVmStatus
      CGNA.Session'subscribeVmStatus'params {CGNA.sink = sink}
      (nacSession nac)
  pure ()

-- ---------------------------------------------------------------------------
-- Chardev streaming
--
-- Each call hands the agent a 'ByteSink' cap to write into (the
-- ring-buffer's replay + live output goes here) and receives back
-- another 'ByteSink' the caller can write to in order to forward
-- bytes into QEMU's chardev (keystrokes / HMP commands).
--
-- The daemon proxies these on behalf of CLI clients today; a
-- future direct-to-client variant can be added without changing
-- this wire shape.

openSerialConsole
  :: NodeAgentClient
  -> Int64
  -> C.Client CGS.ByteSink
  -> IO (Either NodeAgentError (C.Client CGS.ByteSink))
openSerialConsole nac vmId sink = remote $ do
  CGNA.Session'openSerialConsole'results {CGNA.input = inp} <-
    callOn
      #openSerialConsole
      CGNA.Session'openSerialConsole'params
        { CGNA.vmId = vmId
        , CGNA.sink = sink
        }
      (nacSession nac)
  pure inp

openHmpMonitor
  :: NodeAgentClient
  -> Int64
  -> C.Client CGS.ByteSink
  -> IO (Either NodeAgentError (C.Client CGS.ByteSink))
openHmpMonitor nac vmId sink = remote $ do
  CGNA.Session'openHmpMonitor'results {CGNA.input = inp} <-
    callOn
      #openHmpMonitor
      CGNA.Session'openHmpMonitor'params
        { CGNA.vmId = vmId
        , CGNA.sink = sink
        }
      (nacSession nac)
  pure inp

flushSerialConsole
  :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError ())
flushSerialConsole nac vmId = remote $ do
  _ :: C.Parsed CGNA.Session'flushSerialConsole'results <-
    callOn
      #flushSerialConsole
      CGNA.Session'flushSerialConsole'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure ()

flushHmpMonitor
  :: NodeAgentClient -> Int64 -> IO (Either NodeAgentError ())
flushHmpMonitor nac vmId = remote $ do
  _ :: C.Parsed CGNA.Session'flushHmpMonitor'results <-
    callOn
      #flushHmpMonitor
      CGNA.Session'flushHmpMonitor'params {CGNA.vmId = vmId}
      (nacSession nac)
  pure ()

-- ---------------------------------------------------------------------------
-- QMP-mediated runtime changes

-- | Hot-attach a drive via QMP @blockdev-add@ + @device_add@.
-- @driveId@ is the daemon's drive table key; the agent derives
-- QEMU's @node-name@ (@drive-N@) and @id@ (@device-N@) from it.
-- The wire path returns @()@; QMP failures arrive as remote
-- exceptions and surface as 'NodeAgentRemoteError'.
vmAttachDrive
  :: NodeAgentClient
  -> Int64
  -- ^ vmId
  -> Int64
  -- ^ driveId (DB key)
  -> T.Text
  -- ^ resolved disk file path
  -> T.Text
  -- ^ disk format (@"qcow2"@ / @"raw"@ / …)
  -> T.Text
  -- ^ drive interface (@"virtio"@ / @"ide"@ / …)
  -> Bool
  -- ^ read-only
  -> T.Text
  -- ^ media (@"disk"@ / @"cdrom"@)
  -> T.Text
  -- ^ cache mode
  -> Bool
  -- ^ discard
  -> IO (Either NodeAgentError ())
vmAttachDrive nac vmId driveId filePath fmt ifKind ro media cache discard = remote $ do
  let req =
        CGNA.VmAttachDriveReq
          { CGNA.vmId = vmId
          , CGNA.driveId = driveId
          , CGNA.filePath = filePath
          , CGNA.format = fmt
          , CGNA.ifKind = ifKind
          , CGNA.readOnly = ro
          , CGNA.media = media
          , CGNA.cache = cache
          , CGNA.discard = discard
          }
  _ :: C.Parsed CGNA.Session'vmAttachDrive'results <-
    callOn
      #vmAttachDrive
      CGNA.Session'vmAttachDrive'params {CGNA.req = req}
      (nacSession nac)
  pure ()

-- | Hot-detach a drive via QMP @device_del@ + @blockdev-del@
-- (the agent handles the busy-retry).
vmDetachDrive :: NodeAgentClient -> Int64 -> Int64 -> IO (Either NodeAgentError ())
vmDetachDrive nac vmId driveId = remote $ do
  _ :: C.Parsed CGNA.Session'vmDetachDrive'results <-
    callOn
      #vmDetachDrive
      CGNA.Session'vmDetachDrive'params
        { CGNA.vmId = vmId
        , CGNA.driveId = driveId
        }
      (nacSession nac)
  pure ()

-- | Eject the media of a CD-ROM drive (QMP @eject@). The agent
-- verifies the drive is removable via @query-block@ first.
vmEjectMedia :: NodeAgentClient -> Int64 -> Int64 -> IO (Either NodeAgentError ())
vmEjectMedia nac vmId driveId = remote $ do
  _ :: C.Parsed CGNA.Session'vmEjectMedia'results <-
    callOn
      #vmEjectMedia
      CGNA.Session'vmEjectMedia'params
        { CGNA.vmId = vmId
        , CGNA.driveId = driveId
        }
      (nacSession nac)
  pure ()

-- | Replace the media of a CD-ROM drive (QMP
-- @blockdev-change-medium@). The agent verifies the drive is
-- removable via @query-block@ first.
vmChangeMedia
  :: NodeAgentClient
  -> Int64
  -- ^ vmId
  -> Int64
  -- ^ driveId (DB key)
  -> T.Text
  -- ^ resolved disk file path
  -> T.Text
  -- ^ disk format (@"qcow2"@ / @"raw"@ / …)
  -> IO (Either NodeAgentError ())
vmChangeMedia nac vmId driveId filePath fmt = remote $ do
  _ :: C.Parsed CGNA.Session'vmChangeMedia'results <-
    callOn
      #vmChangeMedia
      CGNA.Session'vmChangeMedia'params
        { CGNA.vmId = vmId
        , CGNA.driveId = driveId
        , CGNA.filePath = filePath
        , CGNA.format = fmt
        }
      (nacSession nac)
  pure ()

-- | Probe whether the given AF_VSOCK CID is currently free on
-- the agent's host kernel. Returns 'True' when the kernel
-- reports the CID as available (or when the agent's host has
-- no vhost-vsock device at all).
probeVsockCid :: NodeAgentClient -> Int -> IO (Either NodeAgentError Bool)
probeVsockCid nac cid = remoteWithin 15 $ do
  CGNA.Session'probeVsockCid'results {CGNA.free = free} <-
    callOn
      #probeVsockCid
      CGNA.Session'probeVsockCid'params {CGNA.cid = fromIntegral cid}
      (nacSession nac)
  pure free
