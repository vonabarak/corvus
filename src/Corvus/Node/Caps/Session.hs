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

import qualified Capnp.Gen.Nodeagent as CGNA
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import qualified Corvus.Model as M
import qualified Corvus.Node.CloudInit as NCI
import qualified Corvus.Node.Image as NI
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.Runtime as NR
import qualified Corvus.Process as P
import Corvus.Qemu.Config (QemuConfig, defaultQemuConfig)
import Corvus.Rpc.Common (handleParsed)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
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

-- | Session state. Holds the process-wide 'ProcessLedger' so the
-- process-supervision methods can map PIDs back to
-- 'ProcessHandle's for reaping. Owner is kept for logs.
data SessionCap = SessionCap
  { scOwner :: !Text
  , scProcLedger :: !L.ProcessLedger
  }

newSessionCap :: Text -> L.ProcessLedger -> IO SessionCap
newSessionCap owner procLedger =
  pure SessionCap {scOwner = owner, scProcLedger = procLedger}

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

  -- ---- Process supervision (QEMU + virtiofsd) ----------------------------

  session'processSpawnQemu sc =
    handleParsed $
      \CGNA.Session'processSpawnQemu'params
        { CGNA.vmId = vid
        , CGNA.binary = bin
        , CGNA.args = as
        } -> do
          spawnQemuProcess sc vid (T.unpack bin) (map T.unpack as)

  session'processSpawnVirtiofsd sc =
    handleParsed $
      \CGNA.Session'processSpawnVirtiofsd'params
        { CGNA.binary = bin
        , CGNA.args = as
        , CGNA.socketPath = sp
        , CGNA.waitForSocketTimeoutMs = tmo
        } -> do
          spawnVirtiofsdProcess sc (T.unpack bin) (map T.unpack as) (T.unpack sp) tmo

  session'processStop sc =
    handleParsed $
      \CGNA.Session'processStop'params
        { CGNA.pid = pidI32
        , CGNA.gracefulTimeoutSec = grace
        } -> do
          let pidW = fromIntegral pidI32 :: Word32
          mHandle <- atomically $ L.takeProcess (scProcLedger sc) pidW
          result <-
            runStderrLoggingT $
              P.stopProcess
                ("pid=" <> T.pack (show pidW))
                (CPid (fromIntegral pidW))
                Nothing
                0
                (fromIntegral grace)
          -- Reap the handle if we still had it (waitForProcess can
          -- be called even after the underlying process has exited).
          case mHandle of
            Just ph -> void $ liftIO $ E.try @E.SomeException (waitForProcess ph)
            Nothing -> pure ()
          pure
            CGNA.Session'processStop'results
              { CGNA.result = encodeStopResult result
              }

  session'processIsAlive _ =
    handleParsed $ \CGNA.Session'processIsAlive'params {CGNA.pid = pidI32} -> do
      let pidW = fromIntegral pidI32 :: Word32
      alive <- P.isProcessAlive (CPid (fromIntegral pidW))
      pure CGNA.Session'processIsAlive'results {CGNA.alive = alive}

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
-- Process supervision

-- | A copy of 'Corvus.Qemu.Config.defaultQemuConfig' used for
-- 'NR.createVmRuntimeDir' inside the agent. The daemon's choice of
-- @qcBasePath@ / @qcRuntimeDir@ defaults to the same XDG-derived
-- layout, so a fresh @QemuConfig@ inside the agent produces the
-- same paths.
agentQemuConfig :: QemuConfig
agentQemuConfig = defaultQemuConfig

-- | Spawn QEMU and register the process with the ledger. The
-- agent then forks an internal waiter that reaps the process when
-- it exits.
spawnQemuProcess
  :: SessionCap
  -> Int64
  -> FilePath
  -> [String]
  -> IO (CGNA.Parsed CGNA.Session'processSpawnQemu'results)
spawnQemuProcess sc vmId binary args = do
  -- Ensure VM runtime dir exists before spawning so QEMU can open
  -- its qmp / monitor / serial sockets there.
  _ <- NR.createVmRuntimeDir agentQemuConfig vmId
  r <-
    E.try @E.SomeException $
      createProcess
        (proc binary args)
          { std_out = CreatePipe
          , std_err = CreatePipe
          }
  case r of
    Left e -> throwFailed ("QEMU spawn failed: " <> T.pack (show e))
    Right (_, _, _, ph) -> do
      mPid <- getPid ph
      case mPid of
        Nothing ->
          throwFailed "QEMU spawn returned no PID"
        Just rawPid -> do
          let pidW = fromIntegral rawPid :: Word32
          atomically $ L.insertProcess (scProcLedger sc) pidW ph
          -- Reaper: wait for exit, then drop the entry so a later
          -- processStop is a no-op rather than reaping twice.
          void $ forkIO $ do
            _ <- E.try @E.SomeException (waitForProcess ph)
            atomically $ void $ L.takeProcess (scProcLedger sc) pidW
          pure
            CGNA.Session'processSpawnQemu'results
              { CGNA.pid = fromIntegral pidW :: Int32
              }

-- | Spawn virtiofsd and wait for its socket file to appear before
-- declaring success. Mirrors the loop the daemon used to run
-- inline in @startVirtiofsdForDir@.
spawnVirtiofsdProcess
  :: SessionCap
  -> FilePath
  -> [String]
  -> FilePath
  -> Word32
  -> IO (CGNA.Parsed CGNA.Session'processSpawnVirtiofsd'results)
spawnVirtiofsdProcess sc binary args socketPath waitTimeoutMs = do
  r <-
    E.try @E.SomeException $
      createProcess
        (proc binary args)
          { std_out = CreatePipe
          , std_err = CreatePipe
          }
  case r of
    Left e ->
      pure
        CGNA.Session'processSpawnVirtiofsd'results
          { CGNA.result =
              CGNA.VirtiofsdSpawnResult
                { CGNA.kind = CGNA.VirtiofsdSpawnKind'spawnFailed
                , CGNA.pid = 0
                , CGNA.message = T.pack (show e)
                }
          }
    Right (_, _, _, ph) -> do
      mPid <- getPid ph
      case mPid of
        Nothing -> do
          -- Process started but the OS gave us no PID; clean up
          -- by waiting on the handle.
          void $ E.try @E.SomeException (waitForProcess ph)
          pure
            CGNA.Session'processSpawnVirtiofsd'results
              { CGNA.result =
                  CGNA.VirtiofsdSpawnResult
                    { CGNA.kind = CGNA.VirtiofsdSpawnKind'spawnFailed
                    , CGNA.pid = 0
                    , CGNA.message = "virtiofsd spawn returned no PID"
                    }
              }
        Just rawPid -> do
          let pidW = fromIntegral rawPid :: Word32
          atomically $ L.insertProcess (scProcLedger sc) pidW ph
          -- Reaper for natural exit.
          void $ forkIO $ do
            _ <- E.try @E.SomeException (waitForProcess ph)
            atomically $ void $ L.takeProcess (scProcLedger sc) pidW
          -- Wait for the socket to appear.
          ready <- P.waitForSocketFile socketPath (fromIntegral waitTimeoutMs)
          if ready
            then
              pure
                CGNA.Session'processSpawnVirtiofsd'results
                  { CGNA.result =
                      CGNA.VirtiofsdSpawnResult
                        { CGNA.kind = CGNA.VirtiofsdSpawnKind'success
                        , CGNA.pid = fromIntegral pidW :: Int32
                        , CGNA.message = ""
                        }
                  }
            else do
              -- Socket never appeared; reap the partial child.
              mHandle <- atomically $ L.takeProcess (scProcLedger sc) pidW
              case mHandle of
                Just ph' -> do
                  _ <-
                    runStderrLoggingT $
                      P.stopProcess
                        ("virtiofsd-partial pid=" <> T.pack (show pidW))
                        (CPid (fromIntegral pidW))
                        Nothing
                        0
                        3
                  void $ E.try @E.SomeException (waitForProcess ph')
                Nothing -> pure ()
              pure
                CGNA.Session'processSpawnVirtiofsd'results
                  { CGNA.result =
                      CGNA.VirtiofsdSpawnResult
                        { CGNA.kind = CGNA.VirtiofsdSpawnKind'socketNeverAppeared
                        , CGNA.pid = 0
                        , CGNA.message = T.pack socketPath
                        }
                  }

encodeStopResult :: P.StopResult -> CGNA.Parsed CGNA.ProcessStopResult
encodeStopResult r = case r of
  P.StoppedGracefully ->
    CGNA.ProcessStopResult
      { CGNA.kind = CGNA.ProcessStopKind'stoppedGracefully
      , CGNA.message = ""
      }
  P.StoppedByTerm ->
    CGNA.ProcessStopResult
      { CGNA.kind = CGNA.ProcessStopKind'stoppedByTerm
      , CGNA.message = ""
      }
  P.StoppedByKill ->
    CGNA.ProcessStopResult
      { CGNA.kind = CGNA.ProcessStopKind'stoppedByKill
      , CGNA.message = ""
      }
  P.NotRunning ->
    CGNA.ProcessStopResult
      { CGNA.kind = CGNA.ProcessStopKind'notRunning
      , CGNA.message = ""
      }
  P.StopFailed msg ->
    CGNA.ProcessStopResult
      { CGNA.kind = CGNA.ProcessStopKind'stopFailed
      , CGNA.message = msg
      }
