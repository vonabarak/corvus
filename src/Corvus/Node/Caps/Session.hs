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
import Control.Monad.Logger (LogLevel (..), logDebugN, logInfoN, logWarnN, runStderrLoggingT)
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
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Supervisors (Supervisor)
import System.Directory (getFileSize, renameFile)
import System.Exit (ExitCode (..))
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
  , scTransferTokens :: !NTr.TokenRegistry
  , scTlsConfig :: !(Maybe Tls.TlsConfig)
  }

newSessionCap
  :: Text
  -> Supervisor
  -> L.VmLedger
  -> SP.Subscribers
  -> NGA.GuestAgentConns
  -> TVar (Map.Map Int64 SocketBufferHandle)
  -> TVar (Map.Map Int64 SocketBufferHandle)
  -> NTr.TokenRegistry
  -> Maybe Tls.TlsConfig
  -> IO SessionCap
newSessionCap owner sup vmLedger subs qgaConns serialBufs monitorBufs tokens tlsCfg =
  pure
    SessionCap
      { scOwner = owner
      , scSup = sup
      , scVmLedger = vmLedger
      , scSubs = subs
      , scQgaConns = qgaConns
      , scSerialBuffers = serialBufs
      , scMonitorBuffers = monitorBufs
      , scTransferTokens = tokens
      , scTlsConfig = tlsCfg
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
            } = req
          filePath = T.unpack fpTxt
          nodeName = "drive-" <> T.pack (show drvId)
          deviceId = "device-" <> T.pack (show drvId)
      fmt <- parseFormat fmtTxt
      ifKind <-
        case M.enumFromText ifTxt :: Either Text M.DriveInterface of
          Right k -> pure k
          Left _ -> throwFailed ("unknown drive interface: " <> ifTxt)
      blockResult <-
        NQ.qmpBlockdevAdd agentQemuConfig vid nodeName filePath fmt ro
      case blockResult of
        NQ.QmpSuccess -> do
          deviceResult <-
            NQ.qmpDeviceAddDrive agentQemuConfig vid deviceId nodeName ifKind
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
                NQ.QmpError err -> throwFailed ("blockdev-del: " <> err)
                NQ.QmpConnectionFailed err ->
                  throwFailed ("QMP connect (blockdev-del): " <> err)
            NQ.QmpError err -> throwFailed ("device_del: " <> err)
            NQ.QmpConnectionFailed err ->
              throwFailed ("QMP connect (device_del): " <> err)

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
      mDoneErr <- NTr.waitFileWriter done
      case mDoneErr of
        Nothing -> pure ()
        Just msg ->
          E.throwIO . userError $ "destination writer error: " <> T.unpack msg

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

-- | QEMU @device_del@ completes asynchronously; an immediate
-- @blockdev-del@ on the same node trips a "node N is busy"
-- error. Retry a few times with 100 ms backoff; mirrors the
-- pre-Phase-4 daemon-side retry loop in
-- @Corvus.Handlers.Disk.Attach.qmpBlockdevDelRetry@.
retryBlockdevDel
  :: QemuConfig -> Int64 -> Text -> Int -> IO NQ.QmpResult
retryBlockdevDel cfg vid nodeName = go
  where
    go 0 = NQ.qmpBlockdevDel cfg vid nodeName
    go n = do
      r <- NQ.qmpBlockdevDel cfg vid nodeName
      case r of
        NQ.QmpError err
          | "is in use" `T.isInfixOf` err || "is busy" `T.isInfixOf` err -> do
              threadDelay 100000
              go (n - 1)
        _ -> pure r

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
  runStderrLoggingT $ do
    logInfoN $
      "[nodeagent] vm-" <> tshow vmId <> ": spawning QEMU (" <> T.pack binary <> ")"
    logDebugN $
      "[nodeagent] vm-"
        <> tshow vmId
        <> ": QEMU argv: "
        <> T.pack binary
        <> " "
        <> T.unwords (map (T.pack . show) args)
  spawnResult <-
    E.try @E.SomeException $
      createProcess
        (proc binary args)
          { std_out = CreatePipe
          , std_err = CreatePipe
          }
  case spawnResult of
    Left e -> do
      runStderrLoggingT . logWarnN $
        "[nodeagent] vm-" <> tshow vmId <> ": QEMU spawn failed: " <> T.pack (show e)
      forM_ virtiofsdEntries reapEntryGracefully
      throwFailed ("QEMU spawn failed: " <> T.pack (show e))
    Right (_, mStdoutH, mStderrH, qemuPh) -> do
      mPid <- getPid qemuPh
      case mPid of
        Nothing -> do
          runStderrLoggingT . logWarnN $
            "[nodeagent] vm-" <> tshow vmId <> ": QEMU spawn returned no PID"
          forM_ virtiofsdEntries reapEntryGracefully
          runStderrLoggingT $
            P.waitForProcessBounded "vm-qemu (no-pid path)" 5 qemuPh
          throwFailed "QEMU spawn returned no PID"
        Just rawPid -> do
          let qemuPidW = fromIntegral rawPid :: Word32
          runStderrLoggingT . logInfoN $
            "[nodeagent] vm-" <> tshow vmId <> ": QEMU started pid=" <> tshow qemuPidW
          lastExitVar <- newTVarIO Nothing
          stderrTailVar <- newTVarIO T.empty
          -- Forward QEMU's stdout to the agent log at debug level
          -- so the pipe never fills and back-pressures the child.
          -- Stderr is both ring-buffered (for blame-on-early-exit)
          -- and forwarded line-by-line at debug.
          let qemuLogLabel = "vm-" <> tshow vmId <> "-qemu"
          forM_ mStdoutH $ \h ->
            void $ forkIO $ forwardPipeToLog (qemuLogLabel <> "/stdout") h
          forM_ mStderrH $ \h ->
            void $
              forkIO $
                captureStderrTail (qemuLogLabel <> "/stderr") h stderrTailVar
          let live =
                L.VmLiveState
                  { L.vlsQemuPid = qemuPidW
                  , L.vlsQemuHandle = qemuPh
                  , L.vlsVirtiofsd = virtiofsdEntries
                  , L.vlsLastExitCode = lastExitVar
                  , L.vlsStderrTail = stderrTailVar
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
            runStderrLoggingT . logInfoN $
              "[nodeagent] vm-"
                <> tshow vmId
                <> ": QEMU exited pid="
                <> tshow qemuPidW
                <> " code="
                <> tshow code
          -- First-QGA-ping watcher: fork it off so the RPC can
          -- return as soon as QEMU is alive. The watcher races
          -- the QGA socket against the reaper's exit-code TVar,
          -- so an early QEMU crash surfaces in <1 s. On success
          -- it pushes a single-VM status snapshot to every
          -- subscribed daemon — the daemon's VmStatusSink
          -- promotes its DB row from VmStarting to VmRunning on
          -- receipt. On failure it tears the VM down and pushes
          -- one more snapshot reflecting the errored state.
          --
          -- Pre-split this wait blocked the agent's RPC thread,
          -- which serialized every other vmStart on the same
          -- session — the user-visible "only ~3 VMs can start at
          -- once" bottleneck. Forking here decouples the
          -- session's RPC dispatcher from each VM's individual
          -- first-ping latency.
          when (VS.vsWaitForGuestAgentMs spec > 0) $
            void $
              forkIO $
                runStderrLoggingT $ do
                  result <-
                    liftIO $
                      waitForFirstQgaPing
                        (scQgaConns sc)
                        cfg
                        vmId
                        lastExitVar
                        stderrTailVar
                        (VS.vsWaitForGuestAgentMs spec)
                  case result of
                    Right () -> do
                      logInfoN $
                        "[nodeagent] vm-" <> tshow vmId <> ": first QGA ping landed"
                      liftIO $
                        SP.dispatchVm cfg (scQgaConns sc) (scVmLedger sc) (scSubs sc) vmId
                    Left reason -> do
                      logWarnN $
                        "[nodeagent] vm-"
                          <> tshow vmId
                          <> ": first QGA ping failed: "
                          <> reason
                      let qemuLabel = "vm-" <> tshow vmId <> "-qemu"
                      stopRes <-
                        P.stopProcess
                          qemuLabel
                          (CPid (fromIntegral qemuPidW))
                          Nothing
                          0
                          5
                      case stopRes of
                        P.NotRunning -> pure ()
                        _ -> P.waitForProcessBounded qemuLabel 5 qemuPh
                      -- Push the errored snapshot BEFORE removing
                      -- the entry: 'buildEntry' reads
                      -- 'vlsLastExitCode' (now set by the reaper)
                      -- and emits VmAgentState'errored, which the
                      -- sink translates to setVmError. After
                      -- removal the next 10 s tick reports
                      -- nothing for this VM (the entry list just
                      -- doesn't contain it) — fine for steady
                      -- state.
                      liftIO $
                        SP.dispatchVm cfg (scQgaConns sc) (scVmLedger sc) (scSubs sc) vmId
                      _ <- liftIO $ atomically $ L.removeVm (scVmLedger sc) vmId
                      liftIO $ forM_ virtiofsdEntries reapEntryGracefully
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
  runStderrLoggingT $ do
    logInfoN $
      "[nodeagent] spawning virtiofsd tag="
        <> T.pack tag
        <> " host-path="
        <> VS.vssHostPath d
    logDebugN $
      "[nodeagent] virtiofsd argv: "
        <> T.pack binary
        <> " "
        <> T.unwords (map (T.pack . show) args)
  spawnResult <-
    E.try @E.SomeException $
      createProcess
        (proc binary args)
          { std_out = CreatePipe
          , std_err = CreatePipe
          }
  case spawnResult of
    Left e -> do
      runStderrLoggingT . logWarnN $
        "[nodeagent] virtiofsd tag=" <> T.pack tag <> " spawn failed: " <> T.pack (show e)
      pure $ Left ("virtiofsd " <> T.pack tag <> ": " <> T.pack (show e))
    Right (_, mStdoutH, mStderrH, ph) -> do
      -- Forward virtiofsd's stdout/stderr to the agent log at debug
      -- level. Leaving the pipes unread blocks virtiofsd once the
      -- kernel buffer fills, and — combined with skipping
      -- 'waitForProcess' on the reap path — was the root cause of
      -- the lingering @[virtiofsd] <defunct>@ zombies after VM
      -- teardown.
      let vfsLogLabel = "virtiofsd-" <> T.pack tag
      forM_ mStdoutH $ \h ->
        void $ forkIO $ forwardPipeToLog (vfsLogLabel <> "/stdout") h
      forM_ mStderrH $ \h ->
        void $ forkIO $ forwardPipeToLog (vfsLogLabel <> "/stderr") h
      mPid <- getPid ph
      case mPid of
        Nothing -> do
          runStderrLoggingT . logWarnN $
            "[nodeagent] virtiofsd tag=" <> T.pack tag <> ": no PID after spawn"
          runStderrLoggingT $
            P.waitForProcessBounded
              ("virtiofsd " <> T.pack tag <> " (no-pid)")
              5
              ph
          pure $ Left ("virtiofsd " <> T.pack tag <> ": no PID")
        Just rawPid -> do
          runStderrLoggingT . logInfoN $
            "[nodeagent] virtiofsd tag=" <> T.pack tag <> " started pid=" <> tshow rawPid
          ready <- P.waitForSocketFile socketPath 5000
          if ready
            then pure $ Right (fromIntegral rawPid, ph)
            else do
              let partialLabel = "virtiofsd-partial-" <> T.pack tag
              _ <-
                runStderrLoggingT $
                  P.stopProcess
                    partialLabel
                    (CPid (fromIntegral rawPid))
                    Nothing
                    0
                    3
              -- Always reap, even when stopProcess saw NotRunning
              -- (zombie state still requires our @waitpid@) — see
              -- 'reapEntryGracefully' for the full rationale.
              runStderrLoggingT $ P.waitForProcessBounded partialLabel 5 ph
              pure $ Left ("virtiofsd " <> T.pack tag <> ": socket never appeared")

-- | Best-effort termination + reap of a virtiofsd helper.
--
-- Always call 'waitForProcessBounded' afterwards — including when
-- 'stopProcess' returned 'NotRunning'. That branch fires when the
-- process is already in zombie state (@/proc/<pid>/status@ shows
-- @State: Z@) or its @/proc@ entry has vanished, and in the former
-- case the kernel is still waiting on its parent (us) to
-- @waitpid()@ the entry. Skipping the wait — the old behaviour —
-- left every cleanly-exiting virtiofsd around as
-- @[virtiofsd] <defunct>@. The bounded wait can't deadlock the
-- caller: 'waitForProcessBounded' caps its own wallclock and
-- abandons the handle on timeout.
reapEntryGracefully :: (Word32, ProcessHandle) -> IO ()
reapEntryGracefully (pid, ph) = do
  let label = "virtiofsd pid=" <> tshow pid
  _ <-
    runStderrLoggingT $
      P.stopProcess label (CPid (fromIntegral pid)) Nothing 0 3
  runStderrLoggingT $ P.waitForProcessBounded label 5 ph

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
  :: NGA.GuestAgentConns
  -> QemuConfig
  -> Int64
  -> TVar (Maybe Int)
  -- ^ 'vlsLastExitCode' — set by the reaper the moment QEMU
  -- exits, so we can surface an accurate error instead of timing
  -- out.
  -> TVar T.Text
  -- ^ 'vlsStderrTail' — last ~4 KiB of QEMU's stderr, included
  -- verbatim in the error string when QEMU died early.
  -> Word32
  -- ^ wall-clock budget in milliseconds
  -> IO (Either T.Text ())
  -- ^ 'Right ()' when the first guest-agent ping succeeded;
  -- 'Left reason' on timeout *or* early QEMU exit. The reason
  -- string is intended to flow verbatim into the daemon's task
  -- message and 'vm.error_message'.
waitForFirstQgaPing conns cfg vmId exitVar stderrVar timeoutMs = do
  let budgetSec = fromIntegral timeoutMs / 1000 :: Double
  start <- getMonotonicTime
  loop (start + budgetSec)
  where
    loop deadline = do
      mExit <- readTVarIO exitVar
      case mExit of
        Just code -> Left <$> earlyExitReason code
        Nothing -> do
          ok <- NGA.guestPing conns cfg vmId
          if ok
            then pure $ Right ()
            else do
              -- Re-check the reaper after the (potentially slow)
              -- ping attempt: 'guestPing' has its own ~15 s internal
              -- timeout, so QEMU could have died during the call.
              mExit' <- readTVarIO exitVar
              case mExit' of
                Just code -> Left <$> earlyExitReason code
                Nothing -> do
                  now <- getMonotonicTime
                  if now >= deadline
                    then
                      pure $
                        Left $
                          "guest agent did not respond within "
                            <> tshow timeoutMs
                            <> " ms for vmId "
                            <> tshow vmId
                    else threadDelay 200000 >> loop deadline

    earlyExitReason code = do
      stderr <- readTVarIO stderrVar
      let trimmed = T.strip stderr
          tail' = if T.null trimmed then T.empty else "; stderr tail: " <> trimmed
      pure $
        "QEMU for vmId "
          <> tshow vmId
          <> " exited with code "
          <> tshow code
          <> " before first guest-agent ping"
          <> tail'

-- | Drain a child-process pipe line-by-line, forwarding each line
-- to the agent log at debug level under @label@. Used for every
-- subprocess the agent spawns (QEMU stdout, virtiofsd stdout +
-- stderr, …) so the operator can opt into seeing exactly what the
-- helpers print without leaving the pipes unread (which would
-- back-pressure the child once the kernel buffer fills, and leave
-- zombies behind if the child later exited on its own).
forwardPipeToLog :: Text -> Handle -> IO ()
forwardPipeToLog label h = do
  hSetBuffering h LineBuffering
  let go = do
        eof <- hIsEOF h
        if eof
          then pure ()
          else do
            r <- E.try @E.SomeException (hGetLine h)
            case r of
              Left _ -> pure ()
              Right line -> do
                runStderrLoggingT . logDebugN $
                  "[" <> label <> "] " <> T.pack line
                go
  E.handle (\(_ :: E.SomeException) -> pure ()) go
  E.handle (\(_ :: E.SomeException) -> pure ()) (hClose h)

-- | Tail-capture a child-process pipe into a 'TVar' Text, keeping
-- the last 'stderrTailCapacity' bytes. Used to surface QEMU's own
-- diagnostic output when the wait-for-ping path needs to explain
-- why the VM died. Each line is additionally forwarded to the
-- agent log at debug level under @label@ for live visibility.
captureStderrTail :: Text -> Handle -> TVar T.Text -> IO ()
captureStderrTail label h ringVar = do
  hSetBuffering h LineBuffering
  let go = do
        eof <- hIsEOF h
        if eof
          then pure ()
          else do
            r <- E.try @E.SomeException (hGetLine h)
            case r of
              Left _ -> pure ()
              Right line -> do
                runStderrLoggingT . logDebugN $
                  "[" <> label <> "] " <> T.pack line
                let lineT = T.pack line <> "\n"
                atomically $
                  modifyTVar' ringVar $ \prev ->
                    let combined = prev <> lineT
                        overflow = T.length combined - stderrTailCapacity
                     in if overflow > 0
                          then T.drop overflow combined
                          else combined
                go
  E.handle (\(_ :: E.SomeException) -> pure ()) go
  E.handle (\(_ :: E.SomeException) -> pure ()) (hClose h)

-- | Maximum number of characters retained in 'vlsStderrTail'. Sized
-- to fit QEMU's typical "could not …" / KVM-init / device-init
-- error block (a few hundred bytes) with comfortable headroom; not
-- so large that it inflates the daemon's task-message column.
stderrTailCapacity :: Int
stderrTailCapacity = 4096

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
    Nothing -> do
      runStderrLoggingT . logDebugN $
        "[nodeagent] vmStopGraceful vm-" <> tshow vmId <> ": not in ledger"
      pure
        CGNA.Session'vmStopGraceful'results
          { CGNA.result = encodeVmStopResult VS.VmStopAlreadyStopped ""
          }
    Just live -> do
      runStderrLoggingT . logInfoN $
        "[nodeagent] vmStopGraceful vm-"
          <> tshow vmId
          <> " pid="
          <> tshow (L.vlsQemuPid live)
          <> " timeout="
          <> tshow timeoutSec
          <> "s"
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
    Nothing -> do
      runStderrLoggingT . logDebugN $
        "[nodeagent] vmStopHard vm-" <> tshow vmId <> ": not in ledger"
      pure
        CGNA.Session'vmStopHard'results
          { CGNA.result = encodeVmStopResult VS.VmStopAlreadyStopped ""
          }
    Just live -> do
      runStderrLoggingT . logInfoN $
        "[nodeagent] vmStopHard vm-"
          <> tshow vmId
          <> " pid="
          <> tshow (L.vlsQemuPid live)
      let qemuLabel = "vm-" <> tshow vmId <> "-qemu"
      stopRes <-
        runStderrLoggingT $
          P.stopProcess
            qemuLabel
            (CPid (fromIntegral (L.vlsQemuPid live)))
            Nothing
            0
            5
      case stopRes of
        P.NotRunning -> pure ()
        _ ->
          runStderrLoggingT $
            P.waitForProcessBounded qemuLabel 5 (L.vlsQemuHandle live)
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

-- | Streaming variant of 'handleVmGuestExec'.
--
-- Pushes incremental stdout / stderr bytes to the supplied
-- 'ByteSink' caps as QGA's @guest-exec-status@ drains them. Both
-- sinks are @end()@-ed on completion (success or failure) so the
-- daemon's line buffer can flush any trailing partial line. The
-- returned 'VmGuestExecInfo' carries the exit code only; stdout /
-- stderr are empty because the bytes already flowed through the
-- sinks.
--
-- Sink-write errors (typically: caller dropped its sink) record a
-- flag locally so the loop can stop polling QGA — there's no
-- point doing more guest-side work when nothing receives the
-- output. The return value in that case is a synthetic
-- 'GuestExecError'.
handleVmGuestExecStream
  :: SessionCap
  -> VS.VmGuestExecReq
  -> C.Client CGS.ByteSink
  -> C.Client CGS.ByteSink
  -> IO (CGNA.Parsed CGNA.Session'vmGuestExecStream'results)
handleVmGuestExecStream sc req stdoutCli stderrCli = do
  let vmId = VS.vgeVmId req
      conns = scQgaConns sc
  mLive <- atomically $ L.lookupVm (scVmLedger sc) vmId
  case mLive of
    Nothing -> do
      -- End both sinks before bailing so the daemon's
      -- LineBufferSink doesn't sit waiting on a flush that's
      -- never coming.
      endSinkBest stdoutCli
      endSinkBest stderrCli
      throwFailed ("vmGuestExecStream: unknown vmId " <> tshow vmId)
    Just _ -> do
      let cmd =
            T.intercalate
              " "
              (VS.vgePath req : VS.vgeArgs req)
          maxPolls =
            if VS.vgeTimeoutSec req == 0
              then 600
              else fromIntegral (VS.vgeTimeoutSec req) * 10
          mStdin =
            if BS.null (VS.vgeInputData req)
              then Nothing
              else Just (VS.vgeInputData req)
      brokenRef <- newIORef False
      let pushTo client bs = do
            broken <- readIORef brokenRef
            unless broken $ do
              r <-
                E.try @E.SomeException $
                  callSink
                    #write
                    CGS.ByteSink'write'params {CGS.chunk = bs}
                    client
              case r of
                Left _ -> writeIORef brokenRef True
                Right () -> pure ()
      result <-
        NGA.guestExecStream
          conns
          agentQemuConfig
          vmId
          cmd
          mStdin
          maxPolls
          (pushTo stdoutCli)
          (pushTo stderrCli)
      endSinkBest stdoutCli
      endSinkBest stderrCli
      pure
        CGNA.Session'vmGuestExecStream'results
          { CGNA.info = encodeVmGuestExecInfo result
          }
  where
    endSinkBest client =
      E.handle (\(_ :: E.SomeException) -> pure ()) $
        callSink #end CGS.ByteSink'end'params client

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
