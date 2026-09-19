{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Per-owner session capability for `corvus-nodeagent`.
--
-- Phase 2: disk image operations + cloud-init ISO assembly.
-- All handlers are stateless side-effects on the local host:
-- the daemon supplies absolute paths, the agent runs the
-- corresponding qemu-img / cp / curl / wget / xz / hashing /
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
import qualified Capnp.Gen.Enums as CGE
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Streams as CGS
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import qualified Control.Exception as E
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel (..), logDebugN, logInfoN, logWarnN, runStderrLoggingT)
import qualified Corvus.Model as M
import qualified Corvus.Node.CloudInit as NCI
import qualified Corvus.Node.Command as NC
import qualified Corvus.Node.GuestAgent as NGA
import qualified Corvus.Node.Image as NI
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.Qmp as NQ
import qualified Corvus.Node.Runtime as NR
import qualified Corvus.Node.SnapshotLive as NSL
import Corvus.Node.SocketBuffer (flushBuffer, startSocketBufferThread)
import qualified Corvus.Node.StatusPoller as SP
import qualified Corvus.Node.Transfer as NTr
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.Node.VsockCid as VC
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.Process as P
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Corvus.Rpc.Common (handleParsed, handleParsedAsync)
import Corvus.Rpc.Streams (callSink, runByteSinkRelay)
import qualified Corvus.Tls as Tls
import Corvus.Types (SocketBufferHandle (..))
import qualified Data.ByteString as BS
import Data.Either (lefts, rights)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Supervisors (Supervisor)
import System.Directory (createDirectoryIfMissing, doesPathExist, getFileSize, removeFile, removePathForcibly, renameFile)

import Corvus.Node.Caps.Session.Utils (SessionCap (..), decodeQuiesceMode, encodeDiskInspectInfo, encodeDiskOpResult, flushBufferForVm, isBlockdevBusy, newSessionCap, parseFormat, requireRemovableDrive, retryBlockdevDel, tshow, vmOpLockFor, withVmOpLock)
import Corvus.Node.Caps.Session.Vm
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (BufferMode (..), Handle, hClose, hGetLine, hIsEOF, hSetBuffering)
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
import qualified System.Timeout (timeout)

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
        , CGNA.destFormat = destFormat
        } -> do
          result <- NI.cloneImage (T.unpack src) (T.unpack dst) destFormat
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

  session'snapshotCreateLive sc =
    -- Async dispatch (see 'session'vmGuestExec'): a live snapshot
    -- can fsfreeze the guest and copy qcow2 metadata for many
    -- seconds; it must not stall every other RPC on the session.
    -- Held under the per-VM op lock so it serialises against
    -- start/stop/save on the same VM.
    handleParsedAsync $
      \CGNA.Session'snapshotCreateLive'params
        { CGNA.path = p
        , CGNA.name = n
        , CGNA.vmId = vid
        , CGNA.quiesce = q
        } ->
          withVmOpLock sc vid $ do
            (result, quiesced) <-
              NSL.createSnapshotLive
                (scQgaConns sc)
                agentQemuConfig
                vid
                n
                (T.unpack p)
                (decodeQuiesceMode q)
            pure
              CGNA.Session'snapshotCreateLive'results
                { CGNA.result = encodeDiskOpResult result
                , CGNA.quiesced = quiesced
                }

  session'snapshotDeleteLive _ =
    handleParsed $
      \CGNA.Session'snapshotDeleteLive'params
        { CGNA.path = p
        , CGNA.name = n
        , CGNA.vmId = vid
        } -> do
          result <-
            NSL.deleteSnapshotLive
              agentQemuConfig
              vid
              n
              (T.unpack p)
          pure
            CGNA.Session'snapshotDeleteLive'results
              { CGNA.result = encodeDiskOpResult result
              }

  session'snapshotCreateLiveMany sc =
    -- Same async/op-lock pattern as 'session'snapshotCreateLive'.
    handleParsedAsync $
      \CGNA.Session'snapshotCreateLiveMany'params
        { CGNA.paths = ps
        , CGNA.name = n
        , CGNA.vmId = vid
        , CGNA.quiesce = q
        } ->
          withVmOpLock sc vid $ do
            (result, quiesced) <-
              NSL.createSnapshotLiveMany
                (scQgaConns sc)
                agentQemuConfig
                vid
                n
                (map T.unpack ps)
                (decodeQuiesceMode q)
            pure
              CGNA.Session'snapshotCreateLiveMany'results
                { CGNA.result = encodeDiskOpResult result
                , CGNA.quiesced = quiesced
                }

  session'snapshotCreateWithVmstate sc =
    -- Same async/op-lock pattern as 'session'snapshotCreateLive'.
    -- vmstate save can take several seconds for large RAM; the
    -- VM op lock keeps a parallel `applyVm` or `vmStop` from
    -- racing the save.
    handleParsedAsync $
      \CGNA.Session'snapshotCreateWithVmstate'params
        { CGNA.vmstateDevicePath = vp
        , CGNA.devicePaths = ps
        , CGNA.name = n
        , CGNA.vmId = vid
        } ->
          withVmOpLock sc vid $ do
            result <-
              NSL.createSnapshotWithVmstate
                agentQemuConfig
                vid
                n
                (T.unpack vp)
                (map T.unpack ps)
            pure
              CGNA.Session'snapshotCreateWithVmstate'results
                { CGNA.result = encodeDiskOpResult result
                }

  session'snapshotLoadWithVmstate sc =
    -- Async + op-lock; the caller has already issued QMP `stop`
    -- per the contract documented in 'NSL.loadSnapshotWithVmstate'.
    handleParsedAsync $
      \CGNA.Session'snapshotLoadWithVmstate'params
        { CGNA.vmstateDevicePath = vp
        , CGNA.devicePaths = ps
        , CGNA.name = n
        , CGNA.vmId = vid
        } ->
          withVmOpLock sc vid $ do
            result <-
              NSL.loadSnapshotWithVmstate
                agentQemuConfig
                vid
                n
                (T.unpack vp)
                (map T.unpack ps)
            pure
              CGNA.Session'snapshotLoadWithVmstate'results
                { CGNA.result = encodeDiskOpResult result
                }

  session'snapshotDeleteWithVmstate sc =
    handleParsedAsync $
      \CGNA.Session'snapshotDeleteWithVmstate'params
        { CGNA.devicePaths = ps
        , CGNA.name = n
        , CGNA.vmId = vid
        } ->
          withVmOpLock sc vid $ do
            result <-
              NSL.deleteSnapshotWithVmstate
                agentQemuConfig
                vid
                n
                (map T.unpack ps)
            pure
              CGNA.Session'snapshotDeleteWithVmstate'results
                { CGNA.result = encodeDiskOpResult result
                }

  session'guestSetTime sc =
    -- Single QGA round-trip; uses the persistent QGA connection
    -- pool just like the other guest-agent calls. No VM op lock —
    -- this is a guest-only operation that doesn't touch QEMU
    -- state or the qcow2 file.
    handleParsed $
      \CGNA.Session'guestSetTime'params {CGNA.vmId = vid} -> do
        eResult <-
          NGA.guestSetTime (scQgaConns sc) agentQemuConfig vid
        let result = case eResult of
              Right () -> NI.ImageSuccess
              Left err -> NI.ImageError err
        pure
          CGNA.Session'guestSetTime'results
            { CGNA.result = encodeDiskOpResult result
            }

  -- ---- Download / decompress / hash ----------------------------------------

  session'diskDownload _ =
    handleParsed $
      \CGNA.Session'diskDownload'params
        { CGNA.destPath = d
        , CGNA.url = u
        , CGNA.sink = progressSink
        } -> do
          let onProgress downloaded total = do
                let params =
                      CGS.DiskDownloadSink'progress'params
                        { CGS.downloaded = downloaded
                        , CGS.total = total
                        }
                _ <- E.try (callSink #progress params progressSink) :: IO (Either E.SomeException ())
                pure ()
          result <- NI.downloadImage (T.unpack d) u onProgress
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

  session'diskHash _ =
    handleParsed $ \CGNA.Session'diskHash'params {CGNA.path = p, CGNA.algorithm = a} -> do
      result <- NI.hashFile a (T.unpack p)
      case result of
        Left err -> throwFailed err
        Right h ->
          pure CGNA.Session'diskHash'results {CGNA.hex = h}

  -- ---- VM lifecycle --------------------------------------------------------

  session'vmStart sc =
    -- Async dispatch (see 'session'vmGuestExec'): a cold boot can
    -- block for hundreds of ms spawning virtiofsd + QEMU and
    -- waiting on sockets; under the per-VM op lock so two starts
    -- for the same VM can't orphan each other's processes.
    handleParsedAsync $ \CGNA.Session'vmStart'params {CGNA.spec = wireSpec} ->
      let spec = decodeVmSpec wireSpec
       in withVmOpLock sc (VS.vsVmId spec) (handleVmStart sc spec)

  session'vmStopGraceful sc =
    -- Async dispatch is the headline fix for the per-node wedge:
    -- this handler polls for QEMU exit for up to @timeoutSec@
    -- (default 300 s). Running it inline on the serial 'runServer'
    -- loop blocks EVERY other RPC to this node — probeVsockCid,
    -- vmStatus, disk ops on unrelated VMs — for the whole window.
    -- Fork it so the dispatcher stays free; the per-VM op lock
    -- keeps it ordered against start/save on the same VM.
    -- 'vmStopHard' stays synchronous and lock-free so a reset can
    -- still interrupt a stuck graceful stop.
    handleParsedAsync $
      \CGNA.Session'vmStopGraceful'params
        { CGNA.vmId = vid
        , CGNA.timeoutSec = tmo
        } ->
          withVmOpLock sc vid (handleVmStopGraceful sc vid tmo)

  session'vmStopHard sc =
    handleParsed $
      \CGNA.Session'vmStopHard'params
        { CGNA.vmId = vid
        , CGNA.lifecycleRevision = revision
        , CGNA.hasLifecycleFence = hasFence
        } ->
          handleVmStopHard sc vid (if hasFence then Just revision else Nothing)

  session'vmPause sc =
    handleParsed $ \CGNA.Session'vmPause'params {CGNA.vmId = vid} ->
      handleVmPause sc vid

  session'vmResume sc =
    handleParsed $ \CGNA.Session'vmResume'params {CGNA.vmId = vid} ->
      handleVmResume sc vid

  session'vmSave sc =
    -- Async dispatch (see 'session'vmStopGraceful'): vmSave polls
    -- the outgoing QMP migration for up to 300 s. Under the per-VM
    -- op lock so it serialises against start/stop on the same VM.
    handleParsedAsync $ \CGNA.Session'vmSave'params {CGNA.vmId = vid} ->
      withVmOpLock sc vid (handleVmSave sc vid)

  session'deleteSavedState sc =
    handleParsed $ \CGNA.Session'deleteSavedState'params {CGNA.vmName = name} ->
      handleDeleteSavedState sc name

  session'deleteTpmState sc =
    handleParsed $ \CGNA.Session'deleteTpmState'params {CGNA.vmName = name} ->
      handleDeleteTpmState sc name

  session'vmGuestExec sc =
    -- Async dispatch: a single guest-exec can run for many
    -- minutes (build provisioners are the worst offender), and
    -- 'runServer' on the agent's session cap is a serial loop
    -- — every other RPC on the same daemon→agent connection
    -- (vmStatus, subscribeVmStatus, even disk ops on unrelated
    -- VMs) would queue behind it until the exec returns. Fork
    -- the handler so the dispatcher is free to process the next
    -- call immediately; the per-VM QGA MVar still serialises
    -- \*guest-side* access to one VM, which is the only place
    -- the agent actually needs serialisation.
    handleParsedAsync $ \CGNA.Session'vmGuestExec'params {CGNA.req = wireReq} ->
      handleVmGuestExec sc (decodeVmGuestExecReq wireReq)

  session'vmGuestExecStream sc =
    -- Same async rationale as 'vmGuestExec' — streaming execs
    -- (build provisioners) run even longer than the aggregating
    -- variant, so they MUST NOT block the session dispatcher.
    handleParsedAsync $
      \CGNA.Session'vmGuestExecStream'params
        { CGNA.req = wireReq
        , CGNA.stdoutSink = stdoutCli
        , CGNA.stderrSink = stderrCli
        } ->
          handleVmGuestExecStream
            sc
            (decodeVmGuestExecReq wireReq)
            stdoutCli
            stderrCli

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

  -- ---- QMP-mediated runtime VM changes -------------------------------------

  session'vmAttachDrive _ =
    handleParsed $ \CGNA.Session'vmAttachDrive'params {CGNA.req = req} -> do
      let CGNA.VmAttachDriveReq
            { CGNA.vmId = vid
            , CGNA.driveId = drvId
            , CGNA.filePath = fpTxt
            , CGNA.format = fmtTxt
            , CGNA.ifKind = ifTxt
            , CGNA.readOnly = ro
            , CGNA.media = mediaTxt
            , CGNA.cache = cacheTxt
            , CGNA.discard = discard
            } = req
          filePath = T.unpack fpTxt
          nodeName = "drive-" <> T.pack (show drvId)
          deviceId = "device-" <> T.pack (show drvId)
      fmt <- parseFormat fmtTxt
      ifKind <-
        case M.enumFromText ifTxt :: Either Text M.DriveInterface of
          Right k -> pure k
          Left _ -> throwFailed ("unknown drive interface: " <> ifTxt)
      cache <-
        case M.enumFromText cacheTxt :: Either Text M.CacheType of
          Right c -> pure c
          Left _ -> throwFailed ("unknown drive cache mode: " <> cacheTxt)
      blockResult <-
        NQ.qmpBlockdevAdd agentQemuConfig vid nodeName filePath fmt ro cache discard
      case blockResult of
        NQ.QmpSuccess -> do
          deviceResult <-
            NQ.qmpDeviceAddDrive agentQemuConfig vid deviceId nodeName ifKind mediaTxt cache discard
          case deviceResult of
            NQ.QmpSuccess ->
              pure CGNA.Session'vmAttachDrive'results
            NQ.QmpError err -> do
              -- Best-effort cleanup of the blockdev half we just
              -- added. Surface the original failure regardless.
              _ <-
                E.try @E.SomeException
                  (NQ.qmpBlockdevDel agentQemuConfig vid nodeName)
              throwFailed ("device_add: " <> err)
            NQ.QmpConnectionFailed err ->
              throwFailed ("QMP connect (device_add): " <> err)
        NQ.QmpError err -> throwFailed ("blockdev-add: " <> err)
        NQ.QmpConnectionFailed err ->
          throwFailed ("QMP connect (blockdev-add): " <> err)

  session'vmDetachDrive _ =
    handleParsed $
      \CGNA.Session'vmDetachDrive'params
        { CGNA.vmId = vid
        , CGNA.driveId = drvId
        } -> do
          let nodeName = "drive-" <> T.pack (show drvId)
              deviceId = "device-" <> T.pack (show drvId)
          deviceResult <- NQ.qmpDeviceDel agentQemuConfig vid deviceId
          case deviceResult of
            NQ.QmpSuccess -> do
              -- device_del completes asynchronously; QEMU rejects
              -- a follow-up blockdev-del with "node N is busy" for
              -- a short window. The pre-Phase-4 daemon retried up
              -- to N times with backoff; mirror that.
              blockResult <-
                retryBlockdevDel agentQemuConfig vid nodeName 10
              case blockResult of
                NQ.QmpSuccess ->
                  pure CGNA.Session'vmDetachDrive'results
                -- A guest may take longer than the synchronous
                -- cleanup grace period to acknowledge a PCI
                -- hot-unplug. The frontend is already gone, so
                -- report the detach and keep trying to release its
                -- backend in the agent.
                NQ.QmpError err
                  | isBlockdevBusy err -> do
                      void . forkIO $ do
                        void $
                          retryBlockdevDel agentQemuConfig vid nodeName 600
                      pure CGNA.Session'vmDetachDrive'results
                NQ.QmpError err -> throwFailed ("blockdev-del: " <> err)
                NQ.QmpConnectionFailed err ->
                  throwFailed ("QMP connect (blockdev-del): " <> err)
            NQ.QmpError err -> throwFailed ("device_del: " <> err)
            NQ.QmpConnectionFailed err ->
              throwFailed ("QMP connect (device_del): " <> err)

  session'vmEjectMedia _ =
    handleParsed $
      \CGNA.Session'vmEjectMedia'params
        { CGNA.vmId = vid
        , CGNA.driveId = drvId
        } -> do
          nodeName <- requireRemovableDrive vid drvId
          result <- NQ.qmpEject agentQemuConfig vid nodeName
          case result of
            NQ.QmpSuccess -> pure CGNA.Session'vmEjectMedia'results
            NQ.QmpError err -> throwFailed ("eject: " <> err)
            NQ.QmpConnectionFailed err ->
              throwFailed ("QMP connect (eject): " <> err)

  session'vmChangeMedia _ =
    handleParsed $
      \CGNA.Session'vmChangeMedia'params
        { CGNA.vmId = vid
        , CGNA.driveId = drvId
        , CGNA.filePath = fpTxt
        , CGNA.format = fmtTxt
        } -> do
          nodeName <- requireRemovableDrive vid drvId
          result <- NQ.qmpChangeMedium agentQemuConfig vid nodeName fpTxt (Just fmtTxt)
          case result of
            NQ.QmpSuccess -> pure CGNA.Session'vmChangeMedia'results
            NQ.QmpError err -> throwFailed ("blockdev-change-medium: " <> err)
            NQ.QmpConnectionFailed err ->
              throwFailed ("QMP connect (blockdev-change-medium): " <> err)

  -- ---- Vsock probe ---------------------------------------------------------

  session'probeVsockCid _ =
    handleParsed $
      \CGNA.Session'probeVsockCid'params {CGNA.cid = cid} -> do
        free <- liftIO $ VC.isHostFree (fromIntegral cid)
        pure CGNA.Session'probeVsockCid'results {CGNA.free = free}

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

  -- ---- Inter-agent disk transfer -------------------------------------------

  session'diskOpenRead sc =
    handleParsed $
      \CGNA.Session'diskOpenRead'params {CGNA.path = pTxt} -> do
        let path = T.unpack pTxt
        -- Stat the file + hash it. The hash is computed once, up
        -- front, so the destination can verify the bytes it
        -- received without the source having to recompute on the
        -- fly. Failures (file missing, unreadable) surface as
        -- @throwFailed@ to the daemon.
        eSize <- E.try @E.SomeException (getFileSize path)
        actualSize <- case eSize of
          Right s -> pure s
          Left e ->
            throwFailed
              ("diskOpenRead: stat " <> T.pack path <> " failed: " <> T.pack (show e))
        md5Result <- NI.md5HashFile path
        md5 <- case md5Result of
          Right h -> pure h
          Left err -> throwFailed ("diskOpenRead: md5 failed: " <> err)
        reader <- NTr.newFileReader path
        -- Register the server impl in the process-wide token map
        -- BEFORE exporting it on this session — the destination
        -- agent will re-export it on its own session via
        -- @attachReader@, so the cap returned here is just the
        -- daemon's handle (a separate exported reference to the
        -- same impl).
        token <- NTr.newToken
        NTr.registerReader (scTransferTokens sc) token reader
        readerClient <- C.export @CGNA.DiskReader (scSup sc) reader
        pure
          CGNA.Session'diskOpenRead'results
            { CGNA.reader = readerClient
            , CGNA.token = token
            , CGNA.sizeBytes = fromIntegral actualSize
            , CGNA.md5 = md5
            }

  session'attachReader sc =
    handleParsed $
      \CGNA.Session'attachReader'params {CGNA.token = token} -> do
        mReader <- NTr.redeemReader (scTransferTokens sc) token
        case mReader of
          Nothing ->
            throwFailed "attachReader: unknown or already-consumed token"
          Just reader -> do
            -- Re-export the stored server impl on THIS session's
            -- supervisor; the @C.Client@ originally returned to
            -- the daemon lives on the daemon's connection and
            -- isn't valid to hand back over a different one.
            readerClient <- C.export @CGNA.DiskReader (scSup sc) reader
            pure CGNA.Session'attachReader'results {CGNA.reader = readerClient}

  session'diskImportFromPeer sc =
    handleParsed $
      \CGNA.Session'diskImportFromPeer'params
        { CGNA.destPath = destPathTxt
        , CGNA.peerHost = peerHostTxt
        , CGNA.peerPort = peerPort
        , CGNA.token = token
        , CGNA.expectedBytes = expectedBytes
        , CGNA.expectedMd5 = expectedMd5
        } -> do
          let destPath = T.unpack destPathTxt
              peerHost = T.unpack peerHostTxt
              partPath = destPath <> ".part"
              -- Inter-agent dial: same TLS material, but the peer
              -- presents a corvus-node:<name> cert. We accept any
              -- corvus-node:* peer (CN suffix not pinned).
              mTls = Tls.withPeerExpectation Tls.RoleNode Nothing <$> scTlsConfig sc
          r <-
            E.try @E.SomeException $
              importFromPeer
                sc
                partPath
                peerHost
                (fromIntegral peerPort)
                token
                mTls
          case r of
            Left e -> do
              -- Best-effort cleanup of the partial file.
              _ <- E.try @E.SomeException (NI.deleteImage partPath)
              throwFailed (T.pack (show e))
            Right () -> do
              -- Verify size + md5 against expectations before
              -- renaming into place.
              actualSize <-
                E.handle (\(e :: E.SomeException) -> throwFailed (T.pack (show e))) $
                  getFileSize partPath
              when (fromIntegral actualSize /= expectedBytes) $ do
                _ <- E.try @E.SomeException (NI.deleteImage partPath)
                throwFailed
                  ( "diskImportFromPeer: size mismatch: got "
                      <> T.pack (show actualSize)
                      <> " expected "
                      <> T.pack (show expectedBytes)
                  )
              md5Result <- NI.md5HashFile partPath
              case md5Result of
                Left err -> do
                  _ <- E.try @E.SomeException (NI.deleteImage partPath)
                  throwFailed ("diskImportFromPeer: md5 failed: " <> err)
                Right gotMd5 -> do
                  when (gotMd5 /= expectedMd5) $ do
                    _ <- E.try @E.SomeException (NI.deleteImage partPath)
                    throwFailed
                      ( "diskImportFromPeer: md5 mismatch: got "
                          <> gotMd5
                          <> " expected "
                          <> expectedMd5
                      )
                  -- Promote .part to its final path.
                  renameResult <-
                    E.try @E.SomeException $ renameFile partPath destPath
                  case renameResult of
                    Left e -> throwFailed (T.pack (show e))
                    Right () -> pure ()
          pure CGNA.Session'diskImportFromPeer'results

  session'diskOpenWrite sc =
    handleParsed $ \CGNA.Session'diskOpenWrite'params {CGNA.destPath = destPathTxt} -> do
      -- The daemon resolves and validates this path before asking the node to
      -- open it.  The sink itself writes to a sibling .upload.part and only
      -- renames on end(), so a disconnected client cannot publish a partial
      -- installer medium.
      (sinkImpl, _done) <- NTr.newAtomicFileWriterSink (T.unpack destPathTxt)
      sinkClient <- C.export @CGS.ByteSink (scSup sc) sinkImpl
      pure CGNA.Session'diskOpenWrite'results {CGNA.sink = sinkClient}

-- ---------------------------------------------------------------------------
-- Inter-agent disk import (destination side)
--
-- Opens a fresh @NodeAgentClient@ session to the source agent at
-- @(host, port)@, claims the reader by token via @attachReader@,
-- exports a local 'FileWriterSink' against @partPath@, then runs
-- @reader.pipeInto(sink)@. Blocks until the sink reports
-- completion. The caller is responsible for fsync / rename of
-- @partPath@ to its final location and md5 verification.
importFromPeer
  :: SessionCap
  -> FilePath
  -- ^ partial-file path (typically @<destPath>.part@) the writer
  -- streams bytes into.
  -> String
  -- ^ peer host
  -> Int
  -- ^ peer port
  -> Text
  -- ^ token from @diskOpenRead@
  -> Maybe Tls.TlsConfig
  -- ^ TLS material for the outbound dial. 'Nothing' falls back
  -- to plaintext (the agent was started with @--no-tls@).
  -> IO ()
importFromPeer sc partPath host port token mTls = do
  -- Ensure the parent directory exists before the writer tries
  -- to open the file. The daemon's @disk copy --to-path
  -- subdir/x.qcow2@ flow expects the destination agent to create
  -- @subdir/@ on demand; without this, @openBinaryFile@ inside
  -- 'NTr.newFileWriterSink' fails with ENOENT.
  createDirectoryIfMissing True (takeDirectory partPath)
  (sinkImpl, done) <- NTr.newFileWriterSink partPath
  sinkClient <- C.export @CGS.ByteSink (scSup sc) sinkImpl
  NOA.withNodeAgentClient host port (scOwner sc) mTls $ \case
    Left err ->
      E.throwIO . userError $ "peer dial failed: " <> show err
    Right nac -> do
      readerResult <- NOA.attachReader nac token
      case readerResult of
        Left err ->
          E.throwIO . userError $ "attachReader on peer failed: " <> show err
        Right readerClient -> do
          pipeResult <- NOA.diskReaderPipeInto readerClient sinkClient
          case pipeResult of
            Left err ->
              E.throwIO . userError $ "pipeInto failed: " <> show err
            Right () -> pure ()

-- 'pipeInto' returns when the source side calls 'sink.end',
-- which signals the writer; wait for the writer to flush /
-- close before we return.
-- caller-supplied output sink) to the existing 'runByteSinkRelay'
-- (re-used from the daemon's chardev plumbing), and return the
-- inbound 'ByteSink' cap the caller can write to.

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
