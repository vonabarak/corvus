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
  ( SessionServer (..)
  , SessionCap (..)
  , newSessionCap
  , cleanupDetachedDrive
  , withVmOpLock
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Streams as CGS
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent.STM (TVar, atomically, readTVarIO)
import qualified Control.Exception as E
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Corvus.Model as M
import qualified Corvus.Node.CloudInit as NCI
import qualified Corvus.Node.DirectoryCleanup as DC
import qualified Corvus.Node.GuestAgent as NGA
import qualified Corvus.Node.Image as NI
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.Qmp as NQ
import qualified Corvus.Node.SnapshotLive as NSL
import qualified Corvus.Node.StatusPoller as SP
import qualified Corvus.Node.Transfer as NTr
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.Node.VsockCid as VC
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Rpc.Common (handleParsed, handleParsedAsync)
import Corvus.Rpc.FileTransfer (readSource)
import Corvus.Rpc.Streams (callSink, runByteSinkRelay)
import qualified Corvus.Tls as Tls
import Corvus.Types (SocketBufferHandle (..))
import Corvus.Wire.Errors (showWireError)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Supervisors (supervise)
import System.Directory (createDirectoryIfMissing, getFileSize, renameFile)
import System.Timeout (timeout)

import Corvus.Node.Caps.Session.Utils (SessionCap (..), cleanupDetachedDrive, decodeQuiesceMode, encodeDiskInspectResult, encodeDiskOpResult, flushBufferForVm, isBlockdevBusy, logNodeFailure, newSessionCap, parseFormat, requireRemovableDrive, retryBlockdevDel, traceNodeRequest, tshow, withVmOpLock)
import Corvus.Node.Caps.Session.Vm
import Corvus.Node.Caps.Session.Vm.Balloon (handleVmSetBalloon)
import Corvus.Node.Caps.Session.Vm.Lifecycle (sanitiseVmName)
import Corvus.Node.Caps.Session.Vm.Reboot (handleVmGuestPing, handleVmGuestReboot, handleVmReboot)
import Corvus.Node.Caps.Session.Vm.Screenshot (handleVmScreenshot)
import qualified Corvus.Node.Runtime as NR
import Corvus.Qemu.Config (getEffectiveBasePath)
import System.FilePath (takeDirectory)

newtype SessionServer = SessionServer SessionCap

instance SomeServer SessionServer

instance CGNA.Session'server_ SessionServer where
  session'ping _ =
    handleParsed $ \_ -> pure CGNA.Session'ping'results

  -- ---- Disk image operations -----------------------------------------------

  session'diskCreate (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "diskCreate" $
        \CGNA.Session'diskCreate'params
          { CGNA.path = p
          , CGNA.format = fmt
          , CGNA.size = sz
          } -> do
            format <- parseFormat fmt
            result <- NI.createImage (T.unpack p) format sz
            pure
              CGNA.Session'diskCreate'results
                { CGNA.result = encodeDiskOpResult result
                }

  session'diskCreateOverlay (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "diskCreateOverlay" $
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

  session'diskDelete (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "diskDelete" $ \CGNA.Session'diskDelete'params {CGNA.path = p} -> do
      result <- NI.deleteImage (T.unpack p)
      pure
        CGNA.Session'diskDelete'results
          { CGNA.result = encodeDiskOpResult result
          }

  session'removeDirectoryIfEmpty (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "removeDirectoryIfEmpty" $ \CGNA.Session'removeDirectoryIfEmpty'params {CGNA.path = path, CGNA.protectedBasePath = protectedBase} -> do
      base <- getEffectiveBasePath agentQemuConfig
      DC.removeDirectoryIfEmpty (base : [T.unpack protectedBase | not (T.null protectedBase)]) (T.unpack path)
      pure CGNA.Session'removeDirectoryIfEmpty'results

  session'removeVmDirectoryIfEmpty (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "removeVmDirectoryIfEmpty" $ \CGNA.Session'removeVmDirectoryIfEmpty'params {CGNA.vmName = name} -> do
      case sanitiseVmName name of
        Left err -> throwFailed ("removeVmDirectoryIfEmpty: " <> err)
        Right safeName -> do
          base <- getEffectiveBasePath agentQemuConfig
          path <- NR.getSavedStateFile agentQemuConfig safeName
          DC.removeDirectoryIfEmpty [base] (takeDirectory path)
          pure CGNA.Session'removeVmDirectoryIfEmpty'results

  session'diskResize (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "diskResize" $
        \CGNA.Session'diskResize'params
          { CGNA.path = p
          , CGNA.newSize = sz
          } -> do
            result <- NI.resizeImage (T.unpack p) sz
            pure
              CGNA.Session'diskResize'results
                { CGNA.result = encodeDiskOpResult result
                }

  session'diskRebase (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "diskRebase" $
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

  session'diskClone (SessionServer sc) =
    -- Publication can take minutes; keep status and control RPCs dispatching.
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "diskClone" $
        \CGNA.Session'diskClone'params
          { CGNA.sourcePath = src
          , CGNA.destPath = dst
          , CGNA.destFormat = destFormat
          , CGNA.backingPath = backingPath
          , CGNA.backingFormat = backingFormat
          , CGNA.hasBacking = hasBacking
          , CGNA.compact = compact
          } -> do
            let backing = if hasBacking then Just (T.unpack backingPath, backingFormat) else Nothing
            result <- NI.cloneImage (T.unpack src) (T.unpack dst) destFormat backing compact
            pure
              CGNA.Session'diskClone'results
                { CGNA.result = encodeDiskOpResult result
                }

  session'diskInspect (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "diskInspect" $ \CGNA.Session'diskInspect'params {CGNA.path = p} -> do
      result <- NI.inspectImage (T.unpack p)
      case result of
        Left _ -> logNodeFailure (scQgaConns sc) "diskInspect" "image_inspection_failed"
        Right _ -> pure ()
      pure
        CGNA.Session'diskInspect'results
          { CGNA.result = encodeDiskInspectResult result
          }

  -- ---- Snapshot operations -------------------------------------------------

  session'snapshotCreate (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "snapshotCreate" $
        \CGNA.Session'snapshotCreate'params
          { CGNA.path = p
          , CGNA.name = n
          } -> do
            result <- NI.createSnapshot (T.unpack p) n
            pure
              CGNA.Session'snapshotCreate'results
                { CGNA.result = encodeDiskOpResult result
                }

  session'snapshotDelete (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "snapshotDelete" $
        \CGNA.Session'snapshotDelete'params
          { CGNA.path = p
          , CGNA.name = n
          } -> do
            result <- NI.deleteSnapshot (T.unpack p) n
            pure
              CGNA.Session'snapshotDelete'results
                { CGNA.result = encodeDiskOpResult result
                }

  session'snapshotRollback (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "snapshotRollback" $
        \CGNA.Session'snapshotRollback'params
          { CGNA.path = p
          , CGNA.name = n
          } -> do
            result <- NI.rollbackSnapshot (T.unpack p) n
            pure
              CGNA.Session'snapshotRollback'results
                { CGNA.result = encodeDiskOpResult result
                }

  session'snapshotCreateLive (SessionServer sc) =
    -- Async dispatch (see 'session'vmGuestExec'): a live snapshot
    -- can fsfreeze the guest and copy qcow2 metadata for many
    -- seconds; it must not stall every other RPC on the session.
    -- Held under the per-VM op lock so it serialises against
    -- start/stop/save on the same VM.
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "snapshotCreateLive" $
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

  session'snapshotDeleteLive (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "snapshotDeleteLive" $
        \CGNA.Session'snapshotDeleteLive'params
          { CGNA.path = p
          , CGNA.name = n
          , CGNA.vmId = vid
          } -> withVmOpLock sc vid $ do
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

  session'snapshotCreateWithVmstate (SessionServer sc) =
    -- Same async/op-lock pattern as 'session'snapshotCreateLive'.
    -- vmstate save can take several seconds for large RAM; the
    -- VM op lock keeps a parallel `applyVm` or `vmStop` from
    -- racing the save.
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "snapshotCreateWithVmstate" $
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

  session'snapshotLoadWithVmstate (SessionServer sc) =
    -- Async + op-lock; the caller has already issued QMP `stop`
    -- per the contract documented in 'NSL.loadSnapshotWithVmstate'.
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "snapshotLoadWithVmstate" $
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

  session'snapshotDeleteWithVmstate (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "snapshotDeleteWithVmstate" $
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

  session'guestSetTime (SessionServer sc) =
    -- Single QGA round-trip; uses the persistent QGA connection
    -- pool just like the other guest-agent calls. No VM op lock —
    -- this is a guest-only operation that doesn't touch QEMU
    -- state or the qcow2 file.
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "guestSetTime" $
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

  -- These operations can run for minutes. Fork them so unrelated calls on
  -- the shared agent session (including VM deletion) can still be dispatched.
  session'diskDownload (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "diskDownload" $
        \CGNA.Session'diskDownload'params
          { CGNA.destPath = d
          , CGNA.url = u
          , CGNA.sink = progressSink
          } -> do
            progressActive <- newIORef True
            let onProgress downloaded total = do
                  active <- readIORef progressActive
                  let params =
                        CGS.DiskDownloadSink'progress'params
                          { CGS.downloaded = downloaded
                          , CGS.total = total
                          }
                  when active $ do
                    result <- timeout 5000000 (E.try (callSink #progress params progressSink) :: IO (Either E.SomeException ()))
                    case result of
                      Just (Right ()) -> pure ()
                      _ -> writeIORef progressActive False
            result <- NI.downloadImage (T.unpack d) u onProgress
            pure
              CGNA.Session'diskDownload'results
                { CGNA.result = encodeDiskOpResult result
                }

  session'diskDecompressXz (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "diskDecompressXz" $ \CGNA.Session'diskDecompressXz'params {CGNA.xzPath = p} -> do
      result <- NI.decompressXz (T.unpack p)
      case result of
        Left err -> throwFailed err
        Right final ->
          pure
            CGNA.Session'diskDecompressXz'results
              { CGNA.finalPath = T.pack final
              }

  session'diskSourceIdentity (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "diskSourceIdentity" $ \CGNA.Session'diskSourceIdentity'params {CGNA.path = p} -> do
      result <- NI.sourceIdentity (T.unpack p)
      case result of
        Left err -> do
          logNodeFailure (scQgaConns sc) "diskSourceIdentity" "source_backing_chain_inspection_failed"
          throwFailed err
        Right identity -> pure CGNA.Session'diskSourceIdentity'results {CGNA.identity = identity}

  session'diskHash (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "diskHash" $ \CGNA.Session'diskHash'params {CGNA.path = p, CGNA.algorithm = a} -> do
      result <- NI.hashFile a (T.unpack p)
      case result of
        Left err -> throwFailed err
        Right h ->
          pure CGNA.Session'diskHash'results {CGNA.hex = h}

  -- ---- VM lifecycle --------------------------------------------------------

  session'vmStart (SessionServer sc) =
    -- Async dispatch (see 'session'vmGuestExec'): a cold boot can
    -- block for hundreds of ms spawning virtiofsd + QEMU and
    -- waiting on sockets; under the per-VM op lock so two starts
    -- for the same VM can't orphan each other's processes.
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmStart" $ \CGNA.Session'vmStart'params {CGNA.spec = wireSpec} -> do
      spec <- either (throwFailed . showWireError) pure (decodeVmSpec wireSpec)
      withVmOpLock sc (VS.vsVmId spec) (handleVmStart sc spec)

  session'vmStopGraceful (SessionServer sc) =
    -- Async dispatch is the headline fix for the per-node wedge:
    -- this handler polls for QEMU exit for up to @timeoutSec@
    -- (default 300 s). Running it inline on the serial 'runServer'
    -- loop blocks EVERY other RPC to this node — probeVsockCid,
    -- vmStatus, disk ops on unrelated VMs — for the whole window.
    -- Fork it so the dispatcher stays free; the per-VM op lock
    -- keeps it ordered against start/save on the same VM.
    -- 'vmStopHard' signals before acquiring the lock so a reset can
    -- still interrupt a stuck graceful stop.
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "vmStopGraceful" $
        \CGNA.Session'vmStopGraceful'params
          { CGNA.vmId = vid
          , CGNA.timeoutSec = tmo
          } ->
            withVmOpLock sc vid (handleVmStopGraceful sc vid tmo)

  session'vmStopHard (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "vmStopHard" $
        \CGNA.Session'vmStopHard'params
          { CGNA.vmId = vid
          , CGNA.lifecycleRevision = revision
          , CGNA.hasLifecycleFence = hasFence
          } ->
            handleVmStopHard sc vid (if hasFence then Just revision else Nothing)

  session'vmPause (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmPause" $ \CGNA.Session'vmPause'params {CGNA.vmId = vid} ->
      withVmOpLock sc vid (handleVmPause sc vid)

  session'vmScreenshot (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmScreenshot" $ \CGNA.Session'vmScreenshot'params {CGNA.vmId = vid, CGNA.sink = sink} ->
      handleVmScreenshot sc vid sink

  session'vmSetBalloon (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmSetBalloon" $ \CGNA.Session'vmSetBalloon'params {CGNA.vmId = vid, CGNA.targetBytes = target} ->
      withVmOpLock sc vid (handleVmSetBalloon sc vid target)

  session'vmResume (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmResume" $ \CGNA.Session'vmResume'params {CGNA.vmId = vid} ->
      withVmOpLock sc vid (handleVmResume sc vid)

  session'vmSave (SessionServer sc) =
    -- Async dispatch (see 'session'vmStopGraceful'): vmSave polls
    -- the outgoing QMP migration for up to 300 s. Under the per-VM
    -- op lock so it serialises against start/stop on the same VM.
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmSave" $ \CGNA.Session'vmSave'params {CGNA.vmId = vid} ->
      withVmOpLock sc vid (handleVmSave sc vid)

  session'deleteSavedState (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "deleteSavedState" $ \CGNA.Session'deleteSavedState'params {CGNA.vmName = name} ->
      handleDeleteSavedState sc name

  session'deleteTpmState (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "deleteTpmState" $ \CGNA.Session'deleteTpmState'params {CGNA.vmName = name} ->
      handleDeleteTpmState sc name

  session'prepareTpmMigration (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "prepareTpmMigration" $ \CGNA.Session'prepareTpmMigration'params {CGNA.vmName = name} ->
      handlePrepareTpmMigration sc name

  session'restoreTpmMigration (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "restoreTpmMigration" $ \CGNA.Session'restoreTpmMigration'params {CGNA.vmName = name} ->
      handleRestoreTpmMigration sc name

  session'cleanupTpmMigrationArchive (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "cleanupTpmMigrationArchive" $ \CGNA.Session'cleanupTpmMigrationArchive'params {CGNA.vmName = name} ->
      handleCleanupTpmMigrationArchive sc name

  session'vmGuestWriteFile (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmGuestWriteFile" $ \CGNA.Session'vmGuestWriteFile'params {CGNA.vmId = vid, CGNA.path = path, CGNA.source = source} -> guestFileRpc $ do
      count <- NGA.guestWriteFile (scQgaConns sc) agentQemuConfig vid path (readSource source)
      pure CGNA.Session'vmGuestWriteFile'results {CGNA.bytesTransferred = count}

  session'vmGuestReadFile (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmGuestReadFile" $ \CGNA.Session'vmGuestReadFile'params {CGNA.vmId = vid, CGNA.path = path, CGNA.sink = sink} -> guestFileRpc $ do
      let boundedCall action = do
            result <- timeout 60000000 action
            maybe (throwFailed "guest file sink timed out") pure result
          abort = E.catch (void $ timeout 5000000 $ callSink #abort CGS.ByteSink'abort'params sink) (\(_ :: E.SomeException) -> pure ())
      count <-
        ( do
            bytes <- NGA.guestReadFile (scQgaConns sc) agentQemuConfig vid path (\chunk -> callSink #write CGS.ByteSink'write'params {CGS.chunk = chunk} sink)
            boundedCall (callSink #end CGS.ByteSink'end'params sink)
            pure bytes
          )
          `E.onException` abort
      pure CGNA.Session'vmGuestReadFile'results {CGNA.bytesTransferred = count}

  session'vmGuestSshAddAuthorizedKeys (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmGuestSshAddAuthorizedKeys" $ \CGNA.Session'vmGuestSshAddAuthorizedKeys'params {CGNA.vmId = vid, CGNA.username = username, CGNA.keys = keys, CGNA.reset = reset} -> do
      live <- atomically $ L.lookupVm (scVmLedger sc) vid
      case live of
        Nothing -> throwFailed ("vmGuestSshAddAuthorizedKeys: unknown vmId " <> tshow vid)
        Just _ -> do
          result <- NGA.guestSshAddAuthorizedKeys (scQgaConns sc) agentQemuConfig vid username keys reset
          either throwFailed (const $ pure CGNA.Session'vmGuestSshAddAuthorizedKeys'results) result

  session'vmGuestPing (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmGuestPing" $ \CGNA.Session'vmGuestPing'params {CGNA.vmId = vid} ->
      handleVmGuestPing sc vid

  session'vmGuestReboot (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmGuestReboot" $ \CGNA.Session'vmGuestReboot'params {CGNA.vmId = vid, CGNA.timeoutSec = seconds} ->
      withVmOpLock sc vid (handleVmGuestReboot sc vid seconds)

  session'vmReboot (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmReboot" $ \CGNA.Session'vmReboot'params {CGNA.vmId = vid, CGNA.guestAgent = guestAgent} ->
      withVmOpLock sc vid (handleVmReboot sc vid guestAgent)

  session'vmGuestExec (SessionServer sc) =
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
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmGuestExec" $ \CGNA.Session'vmGuestExec'params {CGNA.req = wireReq} ->
      handleVmGuestExec sc (decodeVmGuestExecReq wireReq)

  session'vmGuestExecStream (SessionServer sc) =
    -- Same async rationale as 'vmGuestExec' — streaming execs
    -- (build provisioners) run even longer than the aggregating
    -- variant, so they MUST NOT block the session dispatcher.
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "vmGuestExecStream" $
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

  session'vmStatus (SessionServer sc) =
    handleParsed $ \CGNA.Session'vmStatus'params {CGNA.vmId = vid} ->
      handleVmStatus sc vid

  session'vmSetSpiceTicket (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "vmSetSpiceTicket" $
        \CGNA.Session'vmSetSpiceTicket'params
          { CGNA.vmId = vid
          , CGNA.password = pw
          , CGNA.ttlSeconds = ttl
          } ->
            withVmOpLock sc vid (handleVmSetSpiceTicket sc vid pw ttl)

  session'subscribeVmStatus (SessionServer sc) =
    handleParsed $ \CGNA.Session'subscribeVmStatus'params {CGNA.sink = sink} -> do
      SP.addSubscriber (scSubs sc) sink
      pure CGNA.Session'subscribeVmStatus'results

  -- ---- Chardev streaming ---------------------------------------------------

  session'openSerialConsole (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "openSerialConsole" $
        \CGNA.Session'openSerialConsole'params {CGNA.vmId = vid, CGNA.sink = sink} -> do
          inputCap <- openChardev (scSerialBuffers sc) sc vid sink
          pure CGNA.Session'openSerialConsole'results {CGNA.input = inputCap}

  session'openHmpMonitor (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "openHmpMonitor" $
        \CGNA.Session'openHmpMonitor'params {CGNA.vmId = vid, CGNA.sink = sink} -> do
          inputCap <- openChardev (scMonitorBuffers sc) sc vid sink
          pure CGNA.Session'openHmpMonitor'results {CGNA.input = inputCap}

  session'flushSerialConsole (SessionServer sc) =
    handleParsed $ \CGNA.Session'flushSerialConsole'params {CGNA.vmId = vid} -> do
      flushBufferForVm (scSerialBuffers sc) vid
      pure CGNA.Session'flushSerialConsole'results

  session'flushHmpMonitor (SessionServer sc) =
    handleParsed $ \CGNA.Session'flushHmpMonitor'params {CGNA.vmId = vid} -> do
      flushBufferForVm (scMonitorBuffers sc) vid
      pure CGNA.Session'flushHmpMonitor'results

  -- ---- QMP-mediated runtime VM changes -------------------------------------

  session'vmAttachDrive (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "vmAttachDrive" $ \CGNA.Session'vmAttachDrive'params {CGNA.req = req} -> do
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
      withVmOpLock sc vid $ do
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

  session'vmDetachDrive (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "vmDetachDrive" $
        \CGNA.Session'vmDetachDrive'params
          { CGNA.vmId = vid
          , CGNA.driveId = drvId
          } -> withVmOpLock sc vid $ do
            let nodeName = "drive-" <> T.pack (show drvId)
                deviceId = "device-" <> T.pack (show drvId)
            original <- atomically $ L.lookupVm (scVmLedger sc) vid
            deviceResult <- NQ.qmpDeviceDel agentQemuConfig vid deviceId
            case deviceResult of
              NQ.QmpSuccess -> do
                blockResult <- retryBlockdevDel agentQemuConfig vid nodeName 10
                case blockResult of
                  NQ.QmpSuccess ->
                    pure CGNA.Session'vmDetachDrive'results
                  NQ.QmpError err | isBlockdevBusy err -> do
                    -- PCI unplug is accepted before the guest acknowledges it.
                    -- Keep delayed backend cleanup owned by this connection and
                    -- guard it against a replacement QEMU runtime.
                    supervise (scSup sc) $ cleanupDetachedDrive agentQemuConfig sc vid original nodeName
                    pure CGNA.Session'vmDetachDrive'results
                  NQ.QmpError err -> throwFailed ("blockdev-del: " <> err)
                  NQ.QmpConnectionFailed err ->
                    throwFailed ("QMP connect (blockdev-del): " <> err)
              NQ.QmpError err -> throwFailed ("device_del: " <> err)
              NQ.QmpConnectionFailed err ->
                throwFailed ("QMP connect (device_del): " <> err)

  session'vmEjectMedia (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "vmEjectMedia" $
        \CGNA.Session'vmEjectMedia'params
          { CGNA.vmId = vid
          , CGNA.driveId = drvId
          } -> withVmOpLock sc vid $ do
            nodeName <- requireRemovableDrive vid drvId
            result <- NQ.qmpEject agentQemuConfig vid nodeName
            case result of
              NQ.QmpSuccess -> pure CGNA.Session'vmEjectMedia'results
              NQ.QmpError err -> throwFailed ("eject: " <> err)
              NQ.QmpConnectionFailed err ->
                throwFailed ("QMP connect (eject): " <> err)

  session'vmChangeMedia (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "vmChangeMedia" $
        \CGNA.Session'vmChangeMedia'params
          { CGNA.vmId = vid
          , CGNA.driveId = drvId
          , CGNA.filePath = fpTxt
          , CGNA.format = fmtTxt
          } -> withVmOpLock sc vid $ do
            nodeName <- requireRemovableDrive vid drvId
            result <- NQ.qmpChangeMedium agentQemuConfig vid nodeName fpTxt (Just fmtTxt)
            case result of
              NQ.QmpSuccess -> pure CGNA.Session'vmChangeMedia'results
              NQ.QmpError err -> throwFailed ("blockdev-change-medium: " <> err)
              NQ.QmpConnectionFailed err ->
                throwFailed ("QMP connect (blockdev-change-medium): " <> err)

  -- ---- Vsock probe ---------------------------------------------------------

  session'probeVsockCid (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "probeVsockCid" $
        \CGNA.Session'probeVsockCid'params {CGNA.cid = cid} -> do
          free <- liftIO $ VC.isHostFree (fromIntegral cid)
          pure CGNA.Session'probeVsockCid'results {CGNA.free = free}

  -- ---- Cloud-init ----------------------------------------------------------

  session'cloudInitGenerateIso (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "cloudInitGenerateIso" $
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

  session'diskOpenRead (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "diskOpenRead" $
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
          NTr.superviseReader (scSup sc) reader
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

  session'attachReader (SessionServer sc) =
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
            NTr.superviseReader (scSup sc) reader
            readerClient <- C.export @CGNA.DiskReader (scSup sc) reader
            pure CGNA.Session'attachReader'results {CGNA.reader = readerClient}

  session'diskImportFromPeer (SessionServer sc) =
    handleParsedAsync $
      traceNodeRequest (scQgaConns sc) "diskImportFromPeer" $
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
            let cleanup = void (E.try @E.IOException (NI.deleteImage partPath))
            ( do
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
              )
              `E.onException` cleanup
            pure CGNA.Session'diskImportFromPeer'results

  session'diskOpenWrite (SessionServer sc) =
    handleParsedAsync $ traceNodeRequest (scQgaConns sc) "diskOpenWrite" $ \CGNA.Session'diskOpenWrite'params {CGNA.destPath = destPathTxt} -> do
      -- The daemon resolves and validates this path before asking the node to
      -- open it.  The sink itself writes to a sibling .upload.part and only
      -- renames on end(), so a disconnected client cannot publish a partial
      -- installer medium.
      sinkImpl <- NTr.newAtomicFileWriterSink (T.unpack destPathTxt)
      NTr.superviseWriter (scSup sc) sinkImpl
      sinkClient <- C.export @CGS.ByteSink (scSup sc) sinkImpl
      pure CGNA.Session'diskOpenWrite'results {CGNA.sink = sinkClient}

-- ---------------------------------------------------------------------------
-- Inter-agent disk import (destination side)
--
-- Opens a fresh @NodeAgentClient@ session to the source agent at
-- @(host, port)@, claims the reader by token via @attachReader@,
-- exports a local 'FileWriterSink' against @partPath@, then runs
-- @reader.pipeInto(sink)@. Blocks until the sink flushes and closes
-- the file. The caller verifies its md5 and renames @partPath@
-- to its final location.
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
  sinkImpl <- NTr.newFileWriterSink partPath
  NTr.superviseWriter (scSup sc) sinkImpl
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
            Left err -> do
              _ <- timeout 1000000 (callSink #abort CGS.ByteSink'abort'params sinkClient)
              E.throwIO . userError $ "pipeInto failed: " <> show err
            Right () -> pure ()

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

-- Preserve native QGA error details across the RPC exception boundary.
guestFileRpc :: IO a -> IO a
guestFileRpc action =
  action `E.catch` \(err :: E.SomeException) ->
    case E.fromException err :: Maybe E.SomeAsyncException of
      Just _ -> E.throwIO err
      Nothing -> throwFailed (T.pack (E.displayException err))
