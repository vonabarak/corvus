{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Per-owner session capability for `corvus-nodeagent`.
--
-- Phase 2: disk image operations + cloud-init ISO assembly.
-- All handlers are stateless side-effects on the local host:
-- the daemon supplies absolute paths, the agent runs the
-- corresponding qemu-img / cp / curl / wget / xz / md5sum /
-- genisoimage subprocess and returns the outcome.
--
-- The 'DiskOpResult' wire type mirrors the agent-side
-- 'Corvus.Node.Image.ImageResult' ADT so the daemon can
-- pattern-match the kind without parsing error strings.
--
-- Subsequent phases extend this cap with VM lifecycle
-- (@applyVm@, @deleteVm@, …), console-stream openers, and
-- status push.
module Corvus.Node.Caps.Session
  ( SessionCap (..)
  , newSessionCap
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Streams as CGS
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import qualified Control.Exception as E
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel (..), logWarnN, runStderrLoggingT)
import qualified Corvus.Model as M
import qualified Corvus.Node.CloudInit as NCI
import qualified Corvus.Node.Command as NC
import qualified Corvus.Node.GuestAgent as NGA
import qualified Corvus.Node.Image as NI
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.Qmp as NQ
import qualified Corvus.Node.Runtime as NR
import Corvus.Node.SocketBuffer (flushBuffer, startSocketBufferThread)
import qualified Corvus.Node.StatusPoller as SP
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.Process as P
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Corvus.Rpc.Common (handleParsed)
import Corvus.Rpc.Streams (runByteSinkRelay)
import Corvus.Types (SocketBufferHandle (..))
import qualified Data.ByteString as BS
import Data.Either (lefts, rights)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import Supervisors (Supervisor)
import System.Exit (ExitCode (..))
import System.Posix.Types (CPid (..))
import System.Process
  ( ProcessHandle
  , StdStream (..)
  , createProcess
  , getPid
  , proc
  , std_err
  , std_out
  , waitForProcess
  )

-- | Session state. Holds:
--
--   * 'scSup' — supervisor used to export server-side caps that
--     handlers create (e.g. the inbound chardev sink returned by
--     @openSerialConsole@).
--   * 'scVmLedger' — vmId-keyed live state for the VM-abstraction
--     methods (vmStart / vmStop* / vmStatus / vmGuestExec / …);
--   * 'scSubs' — registry of 'VmStatusSink' caps the
--     'subscribeVmStatus' handler appends to;
--   * 'scQgaConns' — agent-wide per-VM QGA persistent-socket
--     cache shared by 'vmGuestExec' and the status poller.
--   * 'scSerialBuffers' / 'scMonitorBuffers' — agent-wide
--     per-VM chardev ring-buffer registries the chardev
--     streaming RPCs (@openSerialConsole@, @openHmpMonitor@)
--     read from. Populated during @vmStart@.
data SessionCap = SessionCap
  { scOwner :: !Text
  , scSup :: !Supervisor
  , scVmLedger :: !L.VmLedger
  , scSubs :: !SP.Subscribers
  , scQgaConns :: !NGA.GuestAgentConns
  , scSerialBuffers :: !(TVar (Map.Map Int64 SocketBufferHandle))
  , scMonitorBuffers :: !(TVar (Map.Map Int64 SocketBufferHandle))
  }

newSessionCap
  :: Text
  -> Supervisor
  -> L.VmLedger
  -> SP.Subscribers
  -> NGA.GuestAgentConns
  -> TVar (Map.Map Int64 SocketBufferHandle)
  -> TVar (Map.Map Int64 SocketBufferHandle)
  -> IO SessionCap
newSessionCap owner sup vmLedger subs qgaConns serialBufs monitorBufs =
  pure
    SessionCap
      { scOwner = owner
      , scSup = sup
      , scVmLedger = vmLedger
      , scSubs = subs
      , scQgaConns = qgaConns
      , scSerialBuffers = serialBufs
      , scMonitorBuffers = monitorBufs
      }

instance SomeServer SessionCap

instance CGNA.Session'server_ SessionCap where
  session'ping _ =
    handleParsed $ \_ -> pure CGNA.Session'ping'results

  -- ---- Disk image operations -----------------------------------------------

  session'diskCreate _ =
    handleParsed $
      \CGNA.Session'diskCreate'params
        { CGNA.path = p
        , CGNA.format = fmt
        , CGNA.sizeMb = sz
        } -> do
          format <- parseFormat fmt
          result <- NI.createImage (T.unpack p) format sz
          pure
            CGNA.Session'diskCreate'results
              { CGNA.result = encodeDiskOpResult result
              }

  session'diskCreateOverlay _ =
    handleParsed $
      \CGNA.Session'diskCreateOverlay'params
        { CGNA.overlayPath = ov
        , CGNA.backingPath = bk
        , CGNA.backingFormat = bf
        } -> do
          format <- parseFormat bf
          result <- NI.createOverlay (T.unpack ov) (T.unpack bk) format
          pure
            CGNA.Session'diskCreateOverlay'results
              { CGNA.result = encodeDiskOpResult result
              }

  session'diskDelete _ =
    handleParsed $ \CGNA.Session'diskDelete'params {CGNA.path = p} -> do
      result <- NI.deleteImage (T.unpack p)
      pure
        CGNA.Session'diskDelete'results
          { CGNA.result = encodeDiskOpResult result
          }

  session'diskResize _ =
    handleParsed $
      \CGNA.Session'diskResize'params
        { CGNA.path = p
        , CGNA.newSizeMb = sz
        } -> do
          result <- NI.resizeImage (T.unpack p) sz
          pure
            CGNA.Session'diskResize'results
              { CGNA.result = encodeDiskOpResult result
              }

  session'diskRebase _ =
    handleParsed $
      \CGNA.Session'diskRebase'params
        { CGNA.overlayPath = ov
        , CGNA.newBacking = nb
        , CGNA.newBackingFormat = nbf
        , CGNA.hasNewBacking = hnb
        , CGNA.unsafeUpdate = uu
        } -> do
          mBacking <-
            if hnb
              then do
                f <- parseFormat nbf
                pure $ Just (T.unpack nb, f)
              else pure Nothing
          result <- NI.rebaseImage (T.unpack ov) mBacking uu
          pure
            CGNA.Session'diskRebase'results
              { CGNA.result = encodeDiskOpResult result
              }

  session'diskClone _ =
    handleParsed $
      \CGNA.Session'diskClone'params
        { CGNA.sourcePath = src
        , CGNA.destPath = dst
        } -> do
          result <- NI.cloneImage (T.unpack src) (T.unpack dst)
          pure
            CGNA.Session'diskClone'results
              { CGNA.result = encodeDiskOpResult result
              }

  session'diskInspect _ =
    handleParsed $ \CGNA.Session'diskInspect'params {CGNA.path = p} -> do
      result <- NI.getImageInfo (T.unpack p)
      case result of
        Left err -> throwFailed err
        Right info ->
          pure
            CGNA.Session'diskInspect'results
              { CGNA.info = encodeDiskInspectInfo info
              }

  -- ---- Snapshot operations -------------------------------------------------

  session'snapshotCreate _ =
    handleParsed $
      \CGNA.Session'snapshotCreate'params
        { CGNA.path = p
        , CGNA.name = n
        } -> do
          result <- NI.createSnapshot (T.unpack p) n
          pure
            CGNA.Session'snapshotCreate'results
              { CGNA.result = encodeDiskOpResult result
              }

  session'snapshotDelete _ =
    handleParsed $
      \CGNA.Session'snapshotDelete'params
        { CGNA.path = p
        , CGNA.name = n
        } -> do
          result <- NI.deleteSnapshot (T.unpack p) n
          pure
            CGNA.Session'snapshotDelete'results
              { CGNA.result = encodeDiskOpResult result
              }

  session'snapshotRollback _ =
    handleParsed $
      \CGNA.Session'snapshotRollback'params
        { CGNA.path = p
        , CGNA.name = n
        } -> do
          result <- NI.rollbackSnapshot (T.unpack p) n
          pure
            CGNA.Session'snapshotRollback'results
              { CGNA.result = encodeDiskOpResult result
              }

  -- ---- Download / decompress / hash ----------------------------------------

  session'diskDownload _ =
    handleParsed $
      \CGNA.Session'diskDownload'params
        { CGNA.destPath = d
        , CGNA.url = u
        } -> do
          result <- NI.downloadImage (T.unpack d) u
          pure
            CGNA.Session'diskDownload'results
              { CGNA.result = encodeDiskOpResult result
              }

  session'diskDecompressXz _ =
    handleParsed $ \CGNA.Session'diskDecompressXz'params {CGNA.xzPath = p} -> do
      result <- NI.decompressXz (T.unpack p)
      case result of
        Left err -> throwFailed err
        Right final ->
          pure
            CGNA.Session'diskDecompressXz'results
              { CGNA.finalPath = T.pack final
              }

  session'diskMd5 _ =
    handleParsed $ \CGNA.Session'diskMd5'params {CGNA.path = p} -> do
      result <- NI.md5HashFile (T.unpack p)
      case result of
        Left err -> throwFailed err
        Right h ->
          pure CGNA.Session'diskMd5'results {CGNA.hex = h}

  -- ---- VM lifecycle --------------------------------------------------------

  session'vmStart sc =
    handleParsed $ \CGNA.Session'vmStart'params {CGNA.spec = wireSpec} ->
      handleVmStart sc (decodeVmSpec wireSpec)

  session'vmStopGraceful sc =
    handleParsed $
      \CGNA.Session'vmStopGraceful'params
        { CGNA.vmId = vid
        , CGNA.timeoutSec = tmo
        } ->
          handleVmStopGraceful sc vid tmo

  session'vmStopHard sc =
    handleParsed $ \CGNA.Session'vmStopHard'params {CGNA.vmId = vid} ->
      handleVmStopHard sc vid

  session'vmPause sc =
    handleParsed $ \CGNA.Session'vmPause'params {CGNA.vmId = vid} ->
      handleVmPause sc vid

  session'vmResume sc =
    handleParsed $ \CGNA.Session'vmResume'params {CGNA.vmId = vid} ->
      handleVmResume sc vid

  session'vmGuestExec sc =
    handleParsed $ \CGNA.Session'vmGuestExec'params {CGNA.req = wireReq} ->
      handleVmGuestExec sc (decodeVmGuestExecReq wireReq)

  session'vmStatus sc =
    handleParsed $ \CGNA.Session'vmStatus'params {CGNA.vmId = vid} ->
      handleVmStatus sc vid

  session'vmSetSpiceTicket sc =
    handleParsed $
      \CGNA.Session'vmSetSpiceTicket'params
        { CGNA.vmId = vid
        , CGNA.password = pw
        , CGNA.ttlSeconds = ttl
        } ->
          handleVmSetSpiceTicket sc vid pw ttl

  session'subscribeVmStatus sc =
    handleParsed $ \CGNA.Session'subscribeVmStatus'params {CGNA.sink = sink} -> do
      SP.addSubscriber (scSubs sc) sink
      pure CGNA.Session'subscribeVmStatus'results

  -- ---- Chardev streaming ---------------------------------------------------

  session'openSerialConsole sc =
    handleParsed $
      \CGNA.Session'openSerialConsole'params {CGNA.vmId = vid, CGNA.sink = sink} -> do
        inputCap <- openChardev (scSerialBuffers sc) sc vid sink
        pure CGNA.Session'openSerialConsole'results {CGNA.input = inputCap}

  session'openHmpMonitor sc =
    handleParsed $
      \CGNA.Session'openHmpMonitor'params {CGNA.vmId = vid, CGNA.sink = sink} -> do
        inputCap <- openChardev (scMonitorBuffers sc) sc vid sink
        pure CGNA.Session'openHmpMonitor'results {CGNA.input = inputCap}

  session'flushSerialConsole sc =
    handleParsed $ \CGNA.Session'flushSerialConsole'params {CGNA.vmId = vid} -> do
      flushBufferForVm (scSerialBuffers sc) vid
      pure CGNA.Session'flushSerialConsole'results

  session'flushHmpMonitor sc =
    handleParsed $ \CGNA.Session'flushHmpMonitor'params {CGNA.vmId = vid} -> do
      flushBufferForVm (scMonitorBuffers sc) vid
      pure CGNA.Session'flushHmpMonitor'results

  -- ---- Cloud-init ----------------------------------------------------------

  session'cloudInitGenerateIso _ =
    handleParsed $
      \CGNA.Session'cloudInitGenerateIso'params
        { CGNA.targetDir = td
        , CGNA.userData = ud
        , CGNA.metaData = md
        , CGNA.networkConfig = nc
        , CGNA.hasNetworkConfig = hnc
        } -> do
          let mNet = if hnc then Just nc else Nothing
          result <- NCI.assembleCloudInitIso (T.unpack td) ud md mNet
          case result of
            Left err -> throwFailed err
            Right p ->
              pure
                CGNA.Session'cloudInitGenerateIso'results
                  { CGNA.isoPath = T.pack p
                  }

-- ---------------------------------------------------------------------------
-- Encoders / parsers

-- | Decode the wire-level format string into a daemon-side
-- 'DriveFormat'. The wire uses the same lowercase tokens
-- ("qcow2", "raw", …) the daemon serialises elsewhere.
parseFormat :: Text -> IO M.DriveFormat
parseFormat t = case M.enumFromText t of
  Right f -> pure f
  Left _ -> throwFailed ("unknown disk format: " <> t)

encodeDiskOpResult :: NI.ImageResult -> CGNA.Parsed CGNA.DiskOpResult
encodeDiskOpResult r = case r of
  NI.ImageSuccess ->
    CGNA.DiskOpResult {CGNA.kind = CGNA.DiskOpKind'success, CGNA.message = ""}
  NI.ImageError msg ->
    CGNA.DiskOpResult {CGNA.kind = CGNA.DiskOpKind'errorGeneric, CGNA.message = msg}
  NI.ImageNotFound ->
    CGNA.DiskOpResult {CGNA.kind = CGNA.DiskOpKind'errorNotFound, CGNA.message = ""}
  NI.ImageFormatNotSupported msg ->
    CGNA.DiskOpResult
      { CGNA.kind = CGNA.DiskOpKind'errorFormatUnsupported
      , CGNA.message = msg
      }

encodeDiskInspectInfo :: NI.ImageInfo -> CGNA.Parsed CGNA.DiskInspectInfo
encodeDiskInspectInfo info =
  CGNA.DiskInspectInfo
    { CGNA.format = M.enumToText (NI.iiFormat info)
    , CGNA.virtualSizeMb = NI.iiVirtualSizeMb info
    , CGNA.actualSizeMb = fromMaybe 0 (NI.iiActualSizeMb info)
    , CGNA.hasActualSize = isJust (NI.iiActualSizeMb info)
    , CGNA.snapshots = map encodeDiskSnapshotInfo (NI.iiSnapshots info)
    }

encodeDiskSnapshotInfo :: NI.SnapshotData -> CGNA.Parsed CGNA.DiskSnapshotInfo
encodeDiskSnapshotInfo s =
  CGNA.DiskSnapshotInfo
    { CGNA.id = NI.sdId s
    , CGNA.name = NI.sdName s
    , CGNA.sizeMb = maybe 0 fromIntegral (NI.sdSizeMb s) :: Int64
    , CGNA.hasSize = isJust (NI.sdSizeMb s)
    }

-- ---------------------------------------------------------------------------
-- Chardev streaming helpers
--
-- The agent runs one 'startSocketBufferThread' per chardev per VM,
-- each populating an entry in 'scSerialBuffers' / 'scMonitorBuffers'.
-- The two RPC handlers below look up the entry, hand it (and the
-- caller-supplied output sink) to the existing 'runByteSinkRelay'
-- (re-used from the daemon's chardev plumbing), and return the
-- inbound 'ByteSink' cap the caller can write to.

-- | Ring-buffer capacity for a VM's serial console (1 MiB).
serialBufferCapacity :: Int
serialBufferCapacity = 1048576

-- | Ring-buffer capacity for a VM's HMP monitor scrollback (64 KiB).
monitorBufferCapacity :: Int
monitorBufferCapacity = 65536

-- | Common body of 'openSerialConsole' / 'openHmpMonitor':
-- look up the buffer handle for @vmId@, wire it up via
-- 'runByteSinkRelay', and return the input sink cap.
openChardev
  :: TVar (Map.Map Int64 SocketBufferHandle)
  -> SessionCap
  -> Int64
  -> C.Client CGS.ByteSink
  -> IO (C.Client CGS.ByteSink)
openChardev bufMapVar sc vid sink = do
  bufMap <- readTVarIO bufMapVar
  case Map.lookup vid bufMap of
    Nothing -> throwFailed ("chardev buffer not available for vmId " <> tshow vid)
    Just handle -> runByteSinkRelay (scSup sc) handle sink

-- | Best-effort flush of the buffer for the given vmId; silently
-- no-op if the buffer is absent.
flushBufferForVm
  :: TVar (Map.Map Int64 SocketBufferHandle) -> Int64 -> IO ()
flushBufferForVm bufMapVar vid = do
  bufMap <- readTVarIO bufMapVar
  case Map.lookup vid bufMap of
    Nothing -> pure ()
    Just handle -> flushBuffer (sbhBuffer handle)

-- ---------------------------------------------------------------------------
-- Local QEMU/runtime config used by every VM-abstraction handler.
--
-- A copy of 'Corvus.Qemu.Config.defaultQemuConfig'. The daemon's
-- choice of @qcBasePath@ / @qcRuntimeDir@ defaults to the same
-- XDG-derived layout, so a fresh @QemuConfig@ inside the agent
-- produces the same paths.
agentQemuConfig :: QemuConfig
agentQemuConfig = defaultQemuConfig

-- ---------------------------------------------------------------------------
-- VM abstraction — decoders + handler bodies
-- ---------------------------------------------------------------------------

decodeVmSpec :: CGNA.Parsed CGNA.VmSpec -> VS.VmSpec
decodeVmSpec
  CGNA.VmSpec
    { CGNA.vmId = vid
    , CGNA.name = n
    , CGNA.cpuCount = c
    , CGNA.ramMb = r
    , CGNA.headless = h
    , CGNA.guestAgent = g
    , CGNA.vsockCid = vc
    , CGNA.hasVsockCid = hvc
    , CGNA.spicePort = sp
    , CGNA.hasSpicePort = hsp
    , CGNA.drives = ds
    , CGNA.netIfs = nis
    , CGNA.sharedDirs = sds
    , CGNA.waitForGuestAgentMs = wms
    } =
    VS.VmSpec
      { VS.vsVmId = vid
      , VS.vsName = n
      , VS.vsCpuCount = c
      , VS.vsRamMb = r
      , VS.vsHeadless = h
      , VS.vsGuestAgent = g
      , VS.vsVsockCid = if hvc then Just vc else Nothing
      , VS.vsSpicePort = if hsp then Just sp else Nothing
      , VS.vsDrives = map decodeVmDriveSpec ds
      , VS.vsNetIfs = map decodeVmNetIfSpec nis
      , VS.vsSharedDirs = map decodeVmSharedDirSpec sds
      , VS.vsWaitForGuestAgentMs = wms
      }

decodeVmDriveSpec :: CGNA.Parsed CGNA.VmDriveSpec -> VS.VmDriveSpec
decodeVmDriveSpec
  CGNA.VmDriveSpec
    { CGNA.diskFilePath = p
    , CGNA.format = fmt
    , CGNA.ifKind = ik
    , CGNA.media = md
    , CGNA.readOnly = ro
    , CGNA.cache = ca
    , CGNA.discard = di
    } =
    VS.VmDriveSpec
      { VS.vdsDiskFilePath = p
      , VS.vdsFormat = fmt
      , VS.vdsIfKind = ik
      , VS.vdsMedia = md
      , VS.vdsReadOnly = ro
      , VS.vdsCache = ca
      , VS.vdsDiscard = di
      }

decodeVmNetIfSpec :: CGNA.Parsed CGNA.VmNetIfSpec -> VS.VmNetIfSpec
decodeVmNetIfSpec
  CGNA.VmNetIfSpec
    { CGNA.ifType = it
    , CGNA.hostDevice = hd
    , CGNA.macAddress = ma
    } =
    VS.VmNetIfSpec
      { VS.vnsIfType = it
      , VS.vnsHostDevice = hd
      , VS.vnsMacAddress = ma
      }

decodeVmSharedDirSpec :: CGNA.Parsed CGNA.VmSharedDirSpec -> VS.VmSharedDirSpec
decodeVmSharedDirSpec
  CGNA.VmSharedDirSpec
    { CGNA.hostPath = hp
    , CGNA.tag = tg
    , CGNA.cache = ca
    , CGNA.readOnly = ro
    } =
    VS.VmSharedDirSpec
      { VS.vssHostPath = hp
      , VS.vssTag = tg
      , VS.vssCache = ca
      , VS.vssReadOnly = ro
      }

decodeVmGuestExecReq :: CGNA.Parsed CGNA.VmGuestExecReq -> VS.VmGuestExecReq
decodeVmGuestExecReq
  CGNA.VmGuestExecReq
    { CGNA.vmId = vid
    , CGNA.path = p
    , CGNA.args = as
    , CGNA.captureOutput = co
    , CGNA.inputData = i
    , CGNA.timeoutSec = t
    } =
    VS.VmGuestExecReq
      { VS.vgeVmId = vid
      , VS.vgePath = p
      , VS.vgeArgs = as
      , VS.vgeCaptureOutput = co
      , VS.vgeInputData = i
      , VS.vgeTimeoutSec = t
      }

encodeVmRuntimeInfo :: L.VmLiveState -> CGNA.Parsed CGNA.VmRuntimeInfo
encodeVmRuntimeInfo live =
  CGNA.VmRuntimeInfo
    { CGNA.qemuPid = fromIntegral (L.vlsQemuPid live) :: Int32
    , CGNA.virtiofsdPids =
        [fromIntegral pid :: Int32 | (pid, _) <- L.vlsVirtiofsd live]
    , CGNA.spicePort = L.vlsSpicePort live
    }

encodeVmStopResult :: VS.VmStopKind -> Text -> CGNA.Parsed CGNA.VmStopResult
encodeVmStopResult k m =
  CGNA.VmStopResult
    { CGNA.kind = case k of
        VS.VmStopStopped -> CGNA.VmStopKind'stopped
        VS.VmStopAlreadyStopped -> CGNA.VmStopKind'alreadyStopped
        VS.VmStopTimeout -> CGNA.VmStopKind'timeout
        VS.VmStopFailed -> CGNA.VmStopKind'failed
    , CGNA.message = m
    }

encodeVmAgentStatus
  :: VS.VmAgentState -> Int32 -> Int32 -> CGNA.Parsed CGNA.VmAgentStatus
encodeVmAgentStatus s qpid lec =
  CGNA.VmAgentStatus
    { CGNA.state = case s of
        VS.VmAgentRunning -> CGNA.VmAgentState'running
        VS.VmAgentStopped -> CGNA.VmAgentState'stopped
        VS.VmAgentErrored -> CGNA.VmAgentState'errored
        VS.VmAgentUnknown -> CGNA.VmAgentState'unknown
    , CGNA.qemuPid = qpid
    , CGNA.lastExitCode = lec
    }

encodeVmGuestExecInfo
  :: NGA.GuestExecResult -> CGNA.Parsed CGNA.VmGuestExecInfo
encodeVmGuestExecInfo r = case r of
  NGA.GuestExecSuccess code out err ->
    CGNA.VmGuestExecInfo
      { CGNA.exitCode = fromIntegral code :: Int32
      , CGNA.hasExit = True
      , CGNA.signal = 0
      , CGNA.stdout = TE.encodeUtf8 out
      , CGNA.stderr = TE.encodeUtf8 err
      }
  NGA.GuestExecError err ->
    CGNA.VmGuestExecInfo
      { CGNA.exitCode = -1
      , CGNA.hasExit = False
      , CGNA.signal = 0
      , CGNA.stdout = BS.empty
      , CGNA.stderr = TE.encodeUtf8 err
      }
  NGA.GuestExecConnectionFailed err ->
    CGNA.VmGuestExecInfo
      { CGNA.exitCode = -1
      , CGNA.hasExit = False
      , CGNA.signal = 0
      , CGNA.stdout = BS.empty
      , CGNA.stderr = TE.encodeUtf8 err
      }

-- ---------------------------------------------------------------------------
-- Handler implementations

-- | Spawn QEMU + per-shared-dir virtiofsd helpers, track them in
-- the ledger, fork an exit reaper, optionally block for first
-- QGA ping. Idempotent: a re-applied spec for a vmId already in
-- the ledger is a no-op returning the current runtime info.
handleVmStart
  :: SessionCap -> VS.VmSpec -> IO (CGNA.Parsed CGNA.Session'vmStart'results)
handleVmStart sc spec = do
  let vmId = VS.vsVmId spec
      ledger = scVmLedger sc
  mExisting <- atomically $ L.lookupVm ledger vmId
  case mExisting of
    Just live ->
      pure
        CGNA.Session'vmStart'results {CGNA.info = encodeVmRuntimeInfo live}
    Nothing -> doVmStart sc spec

doVmStart
  :: SessionCap -> VS.VmSpec -> IO (CGNA.Parsed CGNA.Session'vmStart'results)
doVmStart sc spec = do
  let vmId = VS.vsVmId spec
      cfg = agentQemuConfig
  _ <- NR.createVmRuntimeDir cfg vmId
  vmRuntimeDir <- NR.getVmRuntimeDir cfg vmId
  monitorSock <- NR.getMonitorSocket cfg vmId
  qmpSock <- NR.getQmpSocket cfg vmId
  serialSock <- NR.getSerialSocket cfg vmId
  guestAgentSock <- NR.getGuestAgentSocket cfg vmId

  -- 1. Spawn virtiofsd per shared dir
  virtiofsdResults <-
    mapM (spawnVirtiofsdHelper cfg vmRuntimeDir) (VS.vsSharedDirs spec)
  let virtiofsdEntries = rights virtiofsdResults
      virtiofsdErrs = lefts virtiofsdResults
  unless (null virtiofsdErrs) $ do
    forM_ virtiofsdEntries reapEntryGracefully
    throwFailed
      ( "virtiofsd spawn failed for vmId "
          <> tshow vmId
          <> ": "
          <> T.intercalate "; " virtiofsdErrs
      )

  -- 2. Build QEMU argv and spawn
  let (binary, args) =
        NC.buildQemuCommandFromSpec
          cfg
          spec
          monitorSock
          qmpSock
          serialSock
          guestAgentSock
          vmRuntimeDir
  spawnResult <-
    E.try @E.SomeException $
      createProcess
        (proc binary args)
          { std_out = CreatePipe
          , std_err = CreatePipe
          }
  case spawnResult of
    Left e -> do
      forM_ virtiofsdEntries reapEntryGracefully
      throwFailed ("QEMU spawn failed: " <> T.pack (show e))
    Right (_, _, _, qemuPh) -> do
      mPid <- getPid qemuPh
      case mPid of
        Nothing -> do
          forM_ virtiofsdEntries reapEntryGracefully
          void $ E.try @E.SomeException (waitForProcess qemuPh)
          throwFailed "QEMU spawn returned no PID"
        Just rawPid -> do
          let qemuPidW = fromIntegral rawPid :: Word32
          lastExitVar <- newTVarIO Nothing
          let live =
                L.VmLiveState
                  { L.vlsQemuPid = qemuPidW
                  , L.vlsQemuHandle = qemuPh
                  , L.vlsVirtiofsd = virtiofsdEntries
                  , L.vlsLastExitCode = lastExitVar
                  , L.vlsSpicePort = fromMaybe 0 (VS.vsSpicePort spec)
                  }
          atomically $ L.insertVm (scVmLedger sc) vmId live
          -- Start agent-side chardev buffer threads. The threads
          -- wait ~1 s for QEMU to open the socket, connect, then
          -- ring-buffer output until QEMU exits (at which point
          -- they remove themselves from the registry).
          -- Serial only for headless VMs (graphical VMs use SPICE
          -- for input/output); HMP monitor unconditionally.
          when (VS.vsHeadless spec) $
            startSocketBufferThread
              cfg
              vmId
              serialSock
              (scSerialBuffers sc)
              serialBufferCapacity
              "serial"
              LevelInfo
          startSocketBufferThread
            cfg
            vmId
            monitorSock
            (scMonitorBuffers sc)
            monitorBufferCapacity
            "monitor"
            LevelInfo
          -- Reaper: waitForProcess QEMU, fill lastExitCode.
          void $ forkIO $ do
            r <- E.try @E.SomeException (waitForProcess qemuPh)
            let code = case r of
                  Right ExitSuccess -> 0
                  Right (ExitFailure n) -> n
                  Left _ -> 1
            atomically $ writeTVar lastExitVar (Just code)
          -- Optional: block until first QGA ping succeeds.
          when (VS.vsWaitForGuestAgentMs spec > 0) $ do
            ok <-
              waitForFirstQgaPing
                (scQgaConns sc)
                cfg
                vmId
                (VS.vsWaitForGuestAgentMs spec)
            unless ok $ do
              -- Tear down: kill QEMU and virtiofsd, drop ledger
              -- entry, surface as failure.
              _ <- atomically $ L.removeVm (scVmLedger sc) vmId
              _ <-
                runStderrLoggingT $
                  P.stopProcess
                    ("vm-" <> tshow vmId <> "-qemu")
                    (CPid (fromIntegral qemuPidW))
                    Nothing
                    0
                    5
              void $ E.try @E.SomeException (waitForProcess qemuPh)
              forM_ virtiofsdEntries reapEntryGracefully
              throwFailed
                ( "QGA ping timeout for vmId "
                    <> tshow vmId
                    <> " after "
                    <> tshow (VS.vsWaitForGuestAgentMs spec)
                    <> " ms"
                )
          pure
            CGNA.Session'vmStart'results {CGNA.info = encodeVmRuntimeInfo live}

-- | Spawn one virtiofsd helper for a shared dir. Returns the PID
-- + 'ProcessHandle' on success, or an error string on failure
-- (caller cleans up any sibling spawns).
spawnVirtiofsdHelper
  :: QemuConfig
  -> FilePath
  -> VS.VmSharedDirSpec
  -> IO (Either Text (Word32, ProcessHandle))
spawnVirtiofsdHelper cfg vmRuntimeDir d = do
  let tag = T.unpack (VS.vssTag d)
      socketPath = vmRuntimeDir <> "/virtiofsd-" <> tag <> ".sock"
      binary = vfsBinary cfg
      baseArgs =
        [ "--socket-path=" <> socketPath
        , "--shared-dir=" <> T.unpack (VS.vssHostPath d)
        , "--cache=" <> T.unpack (VS.vssCache d)
        , "--sandbox=none"
        ]
      args
        | VS.vssReadOnly d = baseArgs <> ["--readonly"]
        | otherwise = baseArgs
  spawnResult <-
    E.try @E.SomeException $
      createProcess
        (proc binary args)
          { std_out = CreatePipe
          , std_err = CreatePipe
          }
  case spawnResult of
    Left e -> pure $ Left ("virtiofsd " <> T.pack tag <> ": " <> T.pack (show e))
    Right (_, _, _, ph) -> do
      mPid <- getPid ph
      case mPid of
        Nothing -> do
          void $ E.try @E.SomeException (waitForProcess ph)
          pure $ Left ("virtiofsd " <> T.pack tag <> ": no PID")
        Just rawPid -> do
          ready <- P.waitForSocketFile socketPath 5000
          if ready
            then pure $ Right (fromIntegral rawPid, ph)
            else do
              _ <-
                runStderrLoggingT $
                  P.stopProcess
                    ("virtiofsd-partial-" <> T.pack tag)
                    (CPid (fromIntegral rawPid))
                    Nothing
                    0
                    3
              void $ E.try @E.SomeException (waitForProcess ph)
              pure $ Left ("virtiofsd " <> T.pack tag <> ": socket never appeared")

-- | Best-effort termination + reap of a virtiofsd helper.
reapEntryGracefully :: (Word32, ProcessHandle) -> IO ()
reapEntryGracefully (pid, ph) = do
  _ <-
    runStderrLoggingT $
      P.stopProcess
        ("virtiofsd pid=" <> tshow pid)
        (CPid (fromIntegral pid))
        Nothing
        0
        3
  void $ E.try @E.SomeException (waitForProcess ph)

-- | Poll QGA every 200 ms up to @timeoutMs@ ms; return 'True' as
-- soon as one ping succeeds, 'False' on timeout. Used by
-- 'doVmStart' to block until the guest agent inside the VM is
-- alive.
--
-- The 'NGA.GuestAgentConns' argument MUST be the agent-wide cache
-- ('scQgaConns'), not a fresh local map. The successful ping
-- caches a live QGA socket in the cache's per-VM MVar; if we
-- used a private map, that socket would stay live but
-- unreferenced after this function returned, and the status
-- poller's subsequent connect to the same QGA chardev would
-- collide with it (QEMU's chardev has backlog=1) and fail with
-- @EAGAIN / Resource temporarily unavailable@.
waitForFirstQgaPing
  :: NGA.GuestAgentConns -> QemuConfig -> Int64 -> Word32 -> IO Bool
waitForFirstQgaPing conns cfg vmId timeoutMs =
  go (max 0 (fromIntegral timeoutMs) :: Int)
  where
    go remaining
      | remaining <= 0 = pure False
      | otherwise = do
          ok <- NGA.guestPing conns cfg vmId
          if ok
            then pure True
            else do
              threadDelay 200000 -- 200 ms
              go (remaining - 200)

-- | Graceful stop: QMP system_powerdown, then poll the reaper's
-- @vlsLastExitCode@ for up to @timeoutSec@. On exit, also reap
-- virtiofsd helpers and drop the ledger entry.
handleVmStopGraceful
  :: SessionCap
  -> Int64
  -> Word32
  -> IO (CGNA.Parsed CGNA.Session'vmStopGraceful'results)
handleVmStopGraceful sc vmId timeoutSec = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing ->
      pure
        CGNA.Session'vmStopGraceful'results
          { CGNA.result = encodeVmStopResult VS.VmStopAlreadyStopped ""
          }
    Just live -> do
      -- Send both shutdown signals: QGA `guest-shutdown` (the guest
      -- runs its own `poweroff` / `shutdown -h now`) and QMP
      -- `system_powerdown` (ACPI power-button). Together they cover
      -- guests with QGA but no acpid (e.g. early cloud-init), guests
      -- with acpid but no QGA, and guests with both. Either one
      -- exiting the QEMU process is good enough.
      _ <- E.try @E.SomeException (NGA.guestShutdown (scQgaConns sc) agentQemuConfig vmId)
      qmpResult <- NQ.qmpShutdown agentQemuConfig vmId
      case qmpResult of
        NQ.QmpSuccess -> pure ()
        NQ.QmpError err -> runStderrLoggingT $ logQmpErr "system_powerdown" err
        NQ.QmpConnectionFailed err ->
          runStderrLoggingT $ logQmpErr "QMP connect" err
      exited <- pollForExit (L.vlsLastExitCode live) (fromIntegral timeoutSec)
      if exited
        then do
          forM_ (L.vlsVirtiofsd live) reapEntryGracefully
          _ <- atomically $ L.removeVm (scVmLedger sc) vmId
          pure
            CGNA.Session'vmStopGraceful'results
              { CGNA.result = encodeVmStopResult VS.VmStopStopped ""
              }
        else
          pure
            CGNA.Session'vmStopGraceful'results
              { CGNA.result = encodeVmStopResult VS.VmStopTimeout ""
              }
  where
    logQmpErr ctx err =
      logWarnN
        ("[nodeagent] vmStopGraceful " <> tshow vmId <> " " <> ctx <> ": " <> err)

-- | Hard stop: SIGTERM-then-SIGKILL QEMU + every virtiofsd
-- helper. Drops the ledger entry.
handleVmStopHard
  :: SessionCap -> Int64 -> IO (CGNA.Parsed CGNA.Session'vmStopHard'results)
handleVmStopHard sc vmId = do
  mLive <- atomically $ L.removeVm (scVmLedger sc) vmId
  case mLive of
    Nothing ->
      pure
        CGNA.Session'vmStopHard'results
          { CGNA.result = encodeVmStopResult VS.VmStopAlreadyStopped ""
          }
    Just live -> do
      _ <-
        runStderrLoggingT $
          P.stopProcess
            ("vm-" <> tshow vmId <> "-qemu")
            (CPid (fromIntegral (L.vlsQemuPid live)))
            Nothing
            0
            5
      void $ E.try @E.SomeException (waitForProcess (L.vlsQemuHandle live))
      forM_ (L.vlsVirtiofsd live) reapEntryGracefully
      pure
        CGNA.Session'vmStopHard'results
          { CGNA.result = encodeVmStopResult VS.VmStopStopped ""
          }

-- | QMP @stop@ — freeze CPUs.
handleVmPause
  :: SessionCap -> Int64 -> IO (CGNA.Parsed CGNA.Session'vmPause'results)
handleVmPause sc vmId = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> throwFailed ("vmPause: unknown vmId " <> tshow vmId)
    Just _ -> do
      r <- NQ.qmpStop agentQemuConfig vmId
      case r of
        NQ.QmpSuccess -> pure CGNA.Session'vmPause'results
        NQ.QmpError err -> throwFailed ("qmpStop: " <> err)
        NQ.QmpConnectionFailed err ->
          throwFailed ("qmpStop connect: " <> err)

-- | QMP @cont@ — unpause.
handleVmResume
  :: SessionCap -> Int64 -> IO (CGNA.Parsed CGNA.Session'vmResume'results)
handleVmResume sc vmId = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> throwFailed ("vmResume: unknown vmId " <> tshow vmId)
    Just _ -> do
      r <- NQ.qmpContinue agentQemuConfig vmId
      case r of
        NQ.QmpSuccess -> pure CGNA.Session'vmResume'results
        NQ.QmpError err -> throwFailed ("qmpContinue: " <> err)
        NQ.QmpConnectionFailed err ->
          throwFailed ("qmpContinue connect: " <> err)

-- | Execute a command via QGA. The agent locates the QGA socket
-- from the VM's runtime layout; a fresh 'GuestAgentConns' is
-- allocated per call for slice A (slice C introduces an
-- agent-wide persistent cache when the status poller arrives).
handleVmGuestExec
  :: SessionCap
  -> VS.VmGuestExecReq
  -> IO (CGNA.Parsed CGNA.Session'vmGuestExec'results)
handleVmGuestExec sc req = do
  let vmId = VS.vgeVmId req
      conns = scQgaConns sc
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> throwFailed ("vmGuestExec: unknown vmId " <> tshow vmId)
    Just _ -> do
      let cmd =
            T.intercalate
              " "
              (VS.vgePath req : VS.vgeArgs req)
          -- Poll budget: convert vgeTimeoutSec (seconds) into
          -- 100 ms ticks expected by guestExecImpl. A zero value
          -- means "use the agent default" — 60 s, same as the
          -- pre-Phase-4 daemon-side helper.
          maxPolls =
            if VS.vgeTimeoutSec req == 0
              then 600
              else fromIntegral (VS.vgeTimeoutSec req) * 10
      result <-
        if BS.null (VS.vgeInputData req)
          then NGA.guestExecWithTimeout conns agentQemuConfig vmId cmd maxPolls
          else
            NGA.guestExecWithStdin
              conns
              agentQemuConfig
              vmId
              cmd
              (VS.vgeInputData req)
              maxPolls
      pure
        CGNA.Session'vmGuestExec'results
          { CGNA.info = encodeVmGuestExecInfo result
          }

handleVmStatus
  :: SessionCap -> Int64 -> IO (CGNA.Parsed CGNA.Session'vmStatus'results)
handleVmStatus sc vmId = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing ->
      pure
        CGNA.Session'vmStatus'results
          { CGNA.status = encodeVmAgentStatus VS.VmAgentUnknown 0 0
          }
    Just live -> do
      mExit <- readTVarIO (L.vlsLastExitCode live)
      let qpid = fromIntegral (L.vlsQemuPid live) :: Int32
      case mExit of
        Nothing ->
          pure
            CGNA.Session'vmStatus'results
              { CGNA.status = encodeVmAgentStatus VS.VmAgentRunning qpid 0
              }
        Just 0 ->
          pure
            CGNA.Session'vmStatus'results
              { CGNA.status = encodeVmAgentStatus VS.VmAgentStopped qpid 0
              }
        Just code ->
          pure
            CGNA.Session'vmStatus'results
              { CGNA.status =
                  encodeVmAgentStatus
                    VS.VmAgentErrored
                    qpid
                    (fromIntegral code)
              }

handleVmSetSpiceTicket
  :: SessionCap
  -> Int64
  -> Text
  -> Word32
  -> IO (CGNA.Parsed CGNA.Session'vmSetSpiceTicket'results)
handleVmSetSpiceTicket sc vmId password ttlSeconds = do
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> throwFailed ("vmSetSpiceTicket: unknown vmId " <> tshow vmId)
    Just _ -> do
      r1 <- NQ.qmpSetSpicePassword agentQemuConfig vmId password
      case r1 of
        NQ.QmpSuccess -> do
          r2 <-
            NQ.qmpExpireSpicePassword
              agentQemuConfig
              vmId
              (fromIntegral ttlSeconds)
          case r2 of
            NQ.QmpSuccess -> pure CGNA.Session'vmSetSpiceTicket'results
            NQ.QmpError err ->
              throwFailed ("qmpExpireSpicePassword: " <> err)
            NQ.QmpConnectionFailed err ->
              throwFailed ("qmpExpireSpicePassword connect: " <> err)
        NQ.QmpError err -> throwFailed ("qmpSetSpicePassword: " <> err)
        NQ.QmpConnectionFailed err ->
          throwFailed ("qmpSetSpicePassword connect: " <> err)

-- ---------------------------------------------------------------------------
-- Tiny helpers

-- | Poll @vlsLastExitCode@ every 100 ms until non-Nothing or
-- timeout. Returns 'True' when the reaper has filled the var
-- (i.e. QEMU has exited), 'False' on timeout.
pollForExit :: TVar (Maybe Int) -> Int -> IO Bool
pollForExit var timeoutSec = go (max 0 timeoutSec * 10)
  where
    go remaining
      | remaining <= 0 = isJust <$> readTVarIO var
      | otherwise = do
          mExit <- readTVarIO var
          case mExit of
            Just _ -> pure True
            Nothing -> threadDelay 100000 >> go (remaining - 1)

vfsBinary :: QemuConfig -> FilePath
vfsBinary = qcVirtiofsdBinary

tshow :: (Show a) => a -> Text
tshow = T.pack . show
