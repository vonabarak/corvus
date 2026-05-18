{-# LANGUAGE OverloadedStrings #-}

-- | Daemon-side adapter that routes disk + cloud-init operations
-- through the node agent, but returns the same
-- 'Corvus.Node.Image.ImageResult' / 'Corvus.Node.Image.ImageInfo'
-- shapes the existing disk handlers already pattern-match against.
--
-- Each helper reads 'ssNodeAgent' from 'ServerState' and dispatches
-- to "Corvus.NodeAgentClient". If the agent is currently
-- unreachable, the call returns @ImageError "nodeagent unavailable"@
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

import Control.Concurrent.STM (readTVarIO)
import Corvus.Model (DriveFormat, EnumText (..))
import qualified Corvus.Node.Image as NI
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Types (ServerState (..))
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
-- 'NI.ImageError' when the agent is unreachable.
withDiskOp
  :: ServerState
  -> (NOA.NodeAgentClient -> IO (Either NOA.NodeAgentError NOA.DiskOpResult))
  -> IO NI.ImageResult
withDiskOp state call = do
  mAgent <- readTVarIO (ssNodeAgent state)
  case mAgent of
    Nothing -> pure $ NI.ImageError "nodeagent unavailable"
    Just nac -> do
      r <- call nac
      pure $ either agentErr fromDiskOpResult r

-- | Run an agent call that yields a 'Right' payload or a wire
-- error. Bubbles up via 'Either Text'.
withEitherText
  :: ServerState
  -> (NOA.NodeAgentClient -> IO (Either NOA.NodeAgentError a))
  -> IO (Either Text a)
withEitherText state call = do
  mAgent <- readTVarIO (ssNodeAgent state)
  case mAgent of
    Nothing -> pure $ Left "nodeagent unavailable"
    Just nac -> do
      r <- call nac
      pure $ case r of
        Left e -> Left (T.pack (show e))
        Right a -> Right a

-- ---------------------------------------------------------------------------
-- Disk image operations

createImageViaAgent :: ServerState -> FilePath -> DriveFormat -> Int64 -> IO NI.ImageResult
createImageViaAgent state path format sizeMb =
  withDiskOp state $ \nac ->
    NOA.diskCreate nac (T.pack path) (enumToText format) sizeMb

createOverlayViaAgent :: ServerState -> FilePath -> FilePath -> DriveFormat -> IO NI.ImageResult
createOverlayViaAgent state overlayPath backingPath backingFormat =
  withDiskOp state $ \nac ->
    NOA.diskCreateOverlay
      nac
      (T.pack overlayPath)
      (T.pack backingPath)
      (enumToText backingFormat)

deleteImageViaAgent :: ServerState -> FilePath -> IO NI.ImageResult
deleteImageViaAgent state path =
  withDiskOp state $ \nac -> NOA.diskDelete nac (T.pack path)

resizeImageViaAgent :: ServerState -> FilePath -> Int64 -> IO NI.ImageResult
resizeImageViaAgent state path newSizeMb =
  withDiskOp state $ \nac -> NOA.diskResize nac (T.pack path) newSizeMb

rebaseImageViaAgent
  :: ServerState
  -> FilePath
  -> Maybe (FilePath, DriveFormat)
  -> Bool
  -> IO NI.ImageResult
rebaseImageViaAgent state overlayPath mNewBacking unsafeUpdate =
  let wireBacking = fmap (bimap T.pack enumToText) mNewBacking
   in withDiskOp state $ \nac ->
        NOA.diskRebase nac (T.pack overlayPath) wireBacking unsafeUpdate

cloneImageViaAgent :: ServerState -> FilePath -> FilePath -> IO NI.ImageResult
cloneImageViaAgent state src dest =
  withDiskOp state $ \nac ->
    NOA.diskClone nac (T.pack src) (T.pack dest)

getImageInfoViaAgent :: ServerState -> FilePath -> IO (Either Text NI.ImageInfo)
getImageInfoViaAgent state path = do
  r <- withEitherText state $ \nac -> NOA.diskInspect nac (T.pack path)
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
getImageSizeMbViaAgent :: ServerState -> FilePath -> IO (Maybe Int)
getImageSizeMbViaAgent state path = do
  r <- getImageInfoViaAgent state path
  pure $ case r of
    Right info -> Just (fromIntegral (NI.iiVirtualSizeMb info))
    Left _ -> Nothing

-- ---------------------------------------------------------------------------
-- Snapshot operations

createSnapshotViaAgent :: ServerState -> FilePath -> Text -> IO NI.ImageResult
createSnapshotViaAgent state path name =
  withDiskOp state $ \nac -> NOA.snapshotCreate nac (T.pack path) name

deleteSnapshotViaAgent :: ServerState -> FilePath -> Text -> IO NI.ImageResult
deleteSnapshotViaAgent state path name =
  withDiskOp state $ \nac -> NOA.snapshotDelete nac (T.pack path) name

rollbackSnapshotViaAgent :: ServerState -> FilePath -> Text -> IO NI.ImageResult
rollbackSnapshotViaAgent state path name =
  withDiskOp state $ \nac -> NOA.snapshotRollback nac (T.pack path) name

-- | Merging a snapshot is just a delete on qcow2; mirror
-- 'NI.mergeSnapshot' for callers.
mergeSnapshotViaAgent :: ServerState -> FilePath -> Text -> IO NI.ImageResult
mergeSnapshotViaAgent = deleteSnapshotViaAgent

-- ---------------------------------------------------------------------------
-- Download / decompress / hash

downloadImageViaAgent :: ServerState -> FilePath -> Text -> IO NI.ImageResult
downloadImageViaAgent state destPath url =
  withDiskOp state $ \nac -> NOA.diskDownload nac (T.pack destPath) url

decompressXzViaAgent :: ServerState -> FilePath -> IO (Either Text FilePath)
decompressXzViaAgent state xzPath = do
  r <- withEitherText state $ \nac -> NOA.diskDecompressXz nac (T.pack xzPath)
  pure $ fmap T.unpack r

md5HashFileViaAgent :: ServerState -> FilePath -> IO (Either Text Text)
md5HashFileViaAgent state path =
  withEitherText state $ \nac -> NOA.diskMd5 nac (T.pack path)
