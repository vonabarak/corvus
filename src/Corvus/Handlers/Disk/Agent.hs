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

    -- * Download / decompress / hash
  , downloadImageViaAgent
  , decompressXzViaAgent
  , md5HashFileViaAgent
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
  :: ServerState -> M.NodeId -> FilePath -> FilePath -> IO NI.ImageResult
cloneImageViaAgent state nid src dest =
  withDiskOp state nid $ \nac ->
    NOA.diskClone nac (T.pack src) (T.pack dest)

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

-- ---------------------------------------------------------------------------
-- Download / decompress / hash

downloadImageViaAgent :: ServerState -> M.NodeId -> FilePath -> Text -> IO NI.ImageResult
downloadImageViaAgent state nid destPath url =
  withDiskOp state nid $ \nac -> NOA.diskDownload nac (T.pack destPath) url

decompressXzViaAgent :: ServerState -> M.NodeId -> FilePath -> IO (Either Text FilePath)
decompressXzViaAgent state nid xzPath = do
  r <- withEitherText state nid $ \nac -> NOA.diskDecompressXz nac (T.pack xzPath)
  pure $ fmap T.unpack r

md5HashFileViaAgent :: ServerState -> M.NodeId -> FilePath -> IO (Either Text Text)
md5HashFileViaAgent state nid path =
  withEitherText state nid $ \nac -> NOA.diskMd5 nac (T.pack path)
