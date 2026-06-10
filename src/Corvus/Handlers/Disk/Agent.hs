{-# LANGUAGE OverloadedStrings #-}

-- | Daemon-side adapter that routes disk + cloud-init operations
-- through the node agent, but returns the same
-- 'Corvus.Node.Image.ImageResult' / 'Corvus.Node.Image.ImageInfo'
-- shapes the existing disk handlers already pattern-match against.
--
-- Each helper looks up the per-node 'NodeAgentClient' from the
-- 'ssAgents' registry (keyed by 'M.NodeId') and dispatches to
-- "Corvus.NodeAgentClient". If the node is unregistered or its
-- nodeagent is currently disconnected, the call returns
-- @ImageError "nodeagent unavailable"@ (or @ImageError <error>@)
-- (or a 'Left' for inspect-style functions), matching the
-- hard-error contract already in place for @netd unavailable@.
--
-- This module is the daemon's only call path into disk I/O in
-- Phase 2 — the handlers no longer import "Corvus.Node.Image"
-- functions directly (the pure helpers
-- 'detectFormatFromPath' / 'isHttpUrl' / 'parseImageInfo' are
-- the exceptions).
module Corvus.Handlers.Disk.Agent
  ( -- * Disk image operations
    createImageViaAgent
  , createOverlayViaAgent
  , deleteImageViaAgent
  , resizeImageViaAgent
  , rebaseImageViaAgent
  , cloneImageViaAgent
  , getImageInfoViaAgent
  , getImageSizeMbViaAgent

    -- * Snapshot operations
  , createSnapshotViaAgent
  , deleteSnapshotViaAgent
  , rollbackSnapshotViaAgent
  , mergeSnapshotViaAgent
  , createSnapshotViaAgentLive
  , createSnapshotViaAgentLiveMany
  , deleteSnapshotViaAgentLive
  , createSnapshotViaAgentWithVmstate
  , loadSnapshotViaAgentWithVmstate
  , deleteSnapshotViaAgentWithVmstate
  , guestSetTimeViaAgent

    -- * Download / decompress / hash
  , downloadImageViaAgent
  , decompressXzViaAgent
  , md5HashFileViaAgent

    -- * Inter-agent transfer
  , openReadViaAgent
  , importFromPeerViaAgent
  )
where

import Corvus.Model (DriveFormat, EnumText (..))
import qualified Corvus.Model as M
import qualified Corvus.Node.Image as NI
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Types (ServerState, lookupNodeAgent)
import Data.Bifunctor (bimap)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Internals

-- | Translate the agent's 'DiskOpResult' back into the daemon-side
-- 'NI.ImageResult' ADT that every disk handler already
-- pattern-matches against.
fromDiskOpResult :: NOA.DiskOpResult -> NI.ImageResult
fromDiskOpResult (NOA.DiskOpResult kind) = case kind of
  NOA.DiskOpSuccess -> NI.ImageSuccess
  NOA.DiskOpError msg -> NI.ImageError msg
  NOA.DiskOpNotFound -> NI.ImageNotFound
  NOA.DiskOpFormatUnsupported msg -> NI.ImageFormatNotSupported msg

-- | Bridge a @NodeAgentError@ into an 'NI.ImageError'.
agentErr :: NOA.NodeAgentError -> NI.ImageResult
agentErr e = NI.ImageError (T.pack (show e))

-- | Run an agent call that yields a 'DiskOpResult', falling back to
-- 'NI.ImageError' when the agent is unreachable for the given node.
withDiskOp
  :: ServerState
  -> M.NodeId
  -> (NOA.NodeAgentClient -> IO (Either NOA.NodeAgentError NOA.DiskOpResult))
  -> IO NI.ImageResult
withDiskOp state nid call = do
  r <- lookupNodeAgent state nid
  case r of
    Left err -> pure $ NI.ImageError err
    Right nac -> do
      res <- call nac
      pure $ either agentErr fromDiskOpResult res

-- | Run an agent call that yields a 'Right' payload or a wire
-- error. Bubbles up via 'Either Text'.
withEitherText
  :: ServerState
  -> M.NodeId
  -> (NOA.NodeAgentClient -> IO (Either NOA.NodeAgentError a))
  -> IO (Either Text a)
withEitherText state nid call = do
  r <- lookupNodeAgent state nid
  case r of
    Left err -> pure (Left err)
    Right nac -> do
      res <- call nac
      pure $ case res of
        Left e -> Left (T.pack (show e))
        Right a -> Right a

-- ---------------------------------------------------------------------------
-- Disk image operations

createImageViaAgent
  :: ServerState -> M.NodeId -> FilePath -> DriveFormat -> Int64 -> IO NI.ImageResult
createImageViaAgent state nid path format sizeMb =
  withDiskOp state nid $ \nac ->
    NOA.diskCreate nac (T.pack path) (enumToText format) sizeMb

createOverlayViaAgent
  :: ServerState -> M.NodeId -> FilePath -> FilePath -> DriveFormat -> IO NI.ImageResult
createOverlayViaAgent state nid overlayPath backingPath backingFormat =
  withDiskOp state nid $ \nac ->
    NOA.diskCreateOverlay
      nac
      (T.pack overlayPath)
      (T.pack backingPath)
      (enumToText backingFormat)

deleteImageViaAgent :: ServerState -> M.NodeId -> FilePath -> IO NI.ImageResult
deleteImageViaAgent state nid path =
  withDiskOp state nid $ \nac -> NOA.diskDelete nac (T.pack path)

resizeImageViaAgent :: ServerState -> M.NodeId -> FilePath -> Int64 -> IO NI.ImageResult
resizeImageViaAgent state nid path newSizeMb =
  withDiskOp state nid $ \nac -> NOA.diskResize nac (T.pack path) newSizeMb

rebaseImageViaAgent
  :: ServerState
  -> M.NodeId
  -> FilePath
  -> Maybe (FilePath, DriveFormat)
  -> Bool
  -> IO NI.ImageResult
rebaseImageViaAgent state nid overlayPath mNewBacking unsafeUpdate =
  let wireBacking = fmap (bimap T.pack enumToText) mNewBacking
   in withDiskOp state nid $ \nac ->
        NOA.diskRebase nac (T.pack overlayPath) wireBacking unsafeUpdate

cloneImageViaAgent
  :: ServerState
  -> M.NodeId
  -> FilePath
  -> FilePath
  -> DriveFormat
  -- ^ destination format — drives the @qemu-img convert -O@ flag so
  -- a qcow2-source-to-raw-destination clone produces a raw file (not
  -- a qcow2 file masquerading as raw in the DB row).
  -> IO NI.ImageResult
cloneImageViaAgent state nid src dest destFormat =
  withDiskOp state nid $ \nac ->
    NOA.diskClone nac (T.pack src) (T.pack dest) (enumToText destFormat)

getImageInfoViaAgent
  :: ServerState -> M.NodeId -> FilePath -> IO (Either Text NI.ImageInfo)
getImageInfoViaAgent state nid path = do
  r <- withEitherText state nid $ \nac -> NOA.diskInspect nac (T.pack path)
  pure $ case r of
    Left e -> Left e
    Right info -> case enumFromText (NOA.diiFormat info) of
      Left _ -> Left ("unknown disk format: " <> NOA.diiFormat info)
      Right fmt ->
        Right
          NI.ImageInfo
            { NI.iiFormat = fmt
            , NI.iiVirtualSizeMb = NOA.diiVirtualSizeMb info
            , NI.iiActualSizeMb = NOA.diiActualSizeMb info
            , NI.iiSnapshots = map toSnap (NOA.diiSnapshots info)
            }
  where
    toSnap s =
      NI.SnapshotData
        { NI.sdId = NOA.dsiId s
        , NI.sdName = NOA.dsiName s
        , NI.sdSizeMb = fmap fromIntegral (NOA.dsiSizeMb s)
        }

-- | Convenience: returns 'Nothing' on any failure (no-agent
-- included). Matches the existing 'NI.getImageSizeMb' surface
-- the daemon already uses for disk-refresh + register flows.
getImageSizeMbViaAgent :: ServerState -> M.NodeId -> FilePath -> IO (Maybe Int)
getImageSizeMbViaAgent state nid path = do
  r <- getImageInfoViaAgent state nid path
  pure $ case r of
    Right info -> Just (fromIntegral (NI.iiVirtualSizeMb info))
    Left _ -> Nothing

-- ---------------------------------------------------------------------------
-- Snapshot operations

createSnapshotViaAgent :: ServerState -> M.NodeId -> FilePath -> Text -> IO NI.ImageResult
createSnapshotViaAgent state nid path name =
  withDiskOp state nid $ \nac -> NOA.snapshotCreate nac (T.pack path) name

deleteSnapshotViaAgent :: ServerState -> M.NodeId -> FilePath -> Text -> IO NI.ImageResult
deleteSnapshotViaAgent state nid path name =
  withDiskOp state nid $ \nac -> NOA.snapshotDelete nac (T.pack path) name

rollbackSnapshotViaAgent :: ServerState -> M.NodeId -> FilePath -> Text -> IO NI.ImageResult
rollbackSnapshotViaAgent state nid path name =
  withDiskOp state nid $ \nac -> NOA.snapshotRollback nac (T.pack path) name

-- | Merging a snapshot is just a delete on qcow2; mirror
-- 'NI.mergeSnapshot' for callers.
mergeSnapshotViaAgent :: ServerState -> M.NodeId -> FilePath -> Text -> IO NI.ImageResult
mergeSnapshotViaAgent = deleteSnapshotViaAgent

-- | Live snapshot create: routes through the agent's QMP path on
-- a running VM. Returns the 'ImageResult' AND a flag indicating
-- whether QGA fsfreeze actually bracketed the snapshot — the
-- daemon stamps this on the 'Snapshot' DB row.
createSnapshotViaAgentLive
  :: ServerState
  -> M.NodeId
  -> FilePath
  -> Text
  -> Int64
  -- ^ VM ID that currently holds the disk open
  -> NOA.QuiesceMode
  -> IO (NI.ImageResult, Bool)
createSnapshotViaAgentLive state nid path name vmId qmode = do
  r <- lookupNodeAgent state nid
  case r of
    Left err -> pure (NI.ImageError err, False)
    Right nac -> do
      res <- NOA.snapshotCreateLive nac (T.pack path) name vmId qmode
      pure $ case res of
        Left e -> (NI.ImageError (T.pack (show e)), False)
        Right (op, q) -> (fromDiskOpResult op, q)

-- | Atomic multi-disk live snapshot. Routes through the agent's
-- transactional QMP path so either every named snapshot is taken
-- or none of them are. Returns the 'ImageResult' plus the QGA
-- quiesce flag.
createSnapshotViaAgentLiveMany
  :: ServerState
  -> M.NodeId
  -> [FilePath]
  -- ^ qcow2 paths to snapshot atomically
  -> Text
  -- ^ snapshot name (same for every disk)
  -> Int64
  -- ^ bake VM ID that currently holds the disks open
  -> NOA.QuiesceMode
  -> IO (NI.ImageResult, Bool)
createSnapshotViaAgentLiveMany state nid paths name vmId qmode = do
  r <- lookupNodeAgent state nid
  case r of
    Left err -> pure (NI.ImageError err, False)
    Right nac -> do
      res <-
        NOA.snapshotCreateLiveMany
          nac
          (map T.pack paths)
          name
          vmId
          qmode
      pure $ case res of
        Left e -> (NI.ImageError (T.pack (show e)), False)
        Right (op, q) -> (fromDiskOpResult op, q)

-- | Live snapshot delete on a running VM. No quiesce needed.
deleteSnapshotViaAgentLive
  :: ServerState -> M.NodeId -> FilePath -> Text -> Int64 -> IO NI.ImageResult
deleteSnapshotViaAgentLive state nid path name vmId =
  withDiskOp state nid $ \nac ->
    NOA.snapshotDeleteLive nac (T.pack path) name vmId

-- | Full-machine snapshot create. The vmstate lands in the carrier
-- disk's qcow2; sibling block snapshots land in every disk in
-- @devicePaths@ (which MUST include the carrier). Requires
-- QEMU >= 6.0 on the target node; the agent capability-probes and
-- refuses with a clear error otherwise.
createSnapshotViaAgentWithVmstate
  :: ServerState
  -> M.NodeId
  -> FilePath
  -- ^ vmstate carrier disk path
  -> [FilePath]
  -- ^ all writable disk paths to snapshot (must include carrier)
  -> Text
  -- ^ snapshot tag
  -> Int64
  -- ^ VM ID
  -> IO NI.ImageResult
createSnapshotViaAgentWithVmstate state nid vmstatePath devicePaths name vmId =
  withDiskOp state nid $ \nac ->
    NOA.snapshotCreateWithVmstate
      nac
      (T.pack vmstatePath)
      (map T.pack devicePaths)
      name
      vmId

-- | Full-machine snapshot load. Caller MUST ensure the VM's CPUs
-- are paused (QMP @stop@) before invoking. After this returns
-- successfully the caller resumes with QMP @cont@ (typically via
-- 'Corvus.Handlers.Vm' helpers) and then issues
-- 'guestSetTimeViaAgent' to resync the wall clock.
loadSnapshotViaAgentWithVmstate
  :: ServerState
  -> M.NodeId
  -> FilePath
  -- ^ vmstate carrier disk path
  -> [FilePath]
  -- ^ all disk paths that participated in the save
  -> Text
  -- ^ snapshot tag
  -> Int64
  -- ^ VM ID
  -> IO NI.ImageResult
loadSnapshotViaAgentWithVmstate state nid vmstatePath devicePaths name vmId =
  withDiskOp state nid $ \nac ->
    NOA.snapshotLoadWithVmstate
      nac
      (T.pack vmstatePath)
      (map T.pack devicePaths)
      name
      vmId

-- | Full-machine snapshot delete. Removes vmstate AND the sibling
-- block snapshots atomically. Required for vmstate-aware
-- snapshots — the disk-only delete leaves vmstate orphaned in
-- the carrier qcow2.
deleteSnapshotViaAgentWithVmstate
  :: ServerState
  -> M.NodeId
  -> [FilePath]
  -- ^ all disk paths that participated in the snapshot
  -> Text
  -- ^ snapshot tag
  -> Int64
  -- ^ VM ID
  -> IO NI.ImageResult
deleteSnapshotViaAgentWithVmstate state nid devicePaths name vmId =
  withDiskOp state nid $ \nac ->
    NOA.snapshotDeleteWithVmstate
      nac
      (map T.pack devicePaths)
      name
      vmId

-- | Tell QGA to resync the guest's wall clock from the host's
-- hardware clock. Used after a vmstate restore. Best-effort:
-- 'ImageError' if QGA is unreachable; caller logs at WARN.
guestSetTimeViaAgent
  :: ServerState -> M.NodeId -> Int64 -> IO NI.ImageResult
guestSetTimeViaAgent state nid vmId =
  withDiskOp state nid $ \nac -> NOA.guestSetTime nac vmId

-- ---------------------------------------------------------------------------
-- Download / decompress / hash

downloadImageViaAgent
  :: ServerState
  -> M.NodeId
  -> FilePath
  -> Text
  -> Maybe (Int64 -> Int64 -> IO ())
  -- ^ Progress callback: when 'Just', the daemon exports a
  -- 'CGS.DiskDownloadSink' translator and forwards each byte-count
  -- update received from the agent. Passing 'Nothing' suppresses
  -- progress reporting (a no-op sink is still exported on the wire
  -- because the schema requires the param).
  -> IO NI.ImageResult
downloadImageViaAgent state nid destPath url mProgress =
  withDiskOp state nid $ \nac -> NOA.diskDownload nac (T.pack destPath) url mProgress

decompressXzViaAgent :: ServerState -> M.NodeId -> FilePath -> IO (Either Text FilePath)
decompressXzViaAgent state nid xzPath = do
  r <- withEitherText state nid $ \nac -> NOA.diskDecompressXz nac (T.pack xzPath)
  pure $ fmap T.unpack r

md5HashFileViaAgent :: ServerState -> M.NodeId -> FilePath -> IO (Either Text Text)
md5HashFileViaAgent state nid path =
  withEitherText state nid $ \nac -> NOA.diskMd5 nac (T.pack path)

-- ---------------------------------------------------------------------------
-- Inter-agent transfer

-- | Ask the source agent to open @path@ for reading. Returns the
-- 'DiskReader' cap (the daemon holds this so the source treats
-- the transfer as live), an opaque single-use token the
-- destination presents to claim the same reader on its own
-- session, plus the source-side size + md5 for verification.
openReadViaAgent
  :: ServerState
  -> M.NodeId
  -> FilePath
  -> Int
  -- ^ token TTL in seconds; the source evicts the token after
  -- this many seconds if it isn't claimed.
  -> IO (Either Text NOA.DiskOpenReadResult)
openReadViaAgent state nid path ttlSec =
  withEitherText state nid $ \nac ->
    NOA.diskOpenRead nac (T.pack path) (fromIntegral ttlSec)

-- | Ask the destination agent to dial the source agent at
-- @(peerHost, peerPort)@, claim the reader by token, and stream
-- bytes into @destPath@. Blocks until the destination has
-- verified the size + md5 against the daemon-supplied
-- expectations (or aborts with a clear error).
importFromPeerViaAgent
  :: ServerState
  -> M.NodeId
  -- ^ destination node
  -> FilePath
  -- ^ destination path
  -> Text
  -- ^ peer host (source's @nodeHost@)
  -> Int
  -- ^ peer port (source's @nodeAgentPort@)
  -> Text
  -- ^ token from 'openReadViaAgent'
  -> Int64
  -- ^ expected size in bytes
  -> Text
  -- ^ expected md5 hex hash
  -> IO (Either Text ())
importFromPeerViaAgent state nid destPath peerHost peerPort token sz md5 =
  withEitherText state nid $ \nac ->
    NOA.diskImportFromPeer
      nac
      (T.pack destPath)
      peerHost
      (fromIntegral peerPort)
      token
      sz
      md5
