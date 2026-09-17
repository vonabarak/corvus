{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Disk image operations for the nodeagent session cap.
module Corvus.Node.Caps.Session.Disk
  ( -- * Disk handlers
    sessionDiskCreate
  , sessionDiskCreateOverlay
  , sessionDiskDelete
  , sessionDiskResize
  , sessionDiskRebase
  , sessionDiskClone
  , sessionDiskInspect

    -- * Encoders (used by Session.hs instance)
  , parseFormat
  , encodeDiskOpResult
  , encodeDiskInspectInfo
  , encodeDiskSnapshotInfo
  )
where

import qualified Capnp.Gen.Nodeagent as CGNA
import Capnp.Rpc (throwFailed)
import qualified Corvus.Model as M
import Corvus.Node.Caps.Session.Utils (decodeQuiesceMode)
import qualified Corvus.Node.Image as NI
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T

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

-- | Handler implementations for disk operations.
-- These are called from Session's'server_ instance via re-export.
sessionDiskCreate
  :: CGNA.Parsed CGNA.Session'diskCreate'params
  -> IO (CGNA.Parsed CGNA.Session'diskCreate'results)
sessionDiskCreate
  CGNA.Session'diskCreate'params
    { CGNA.path = p
    , CGNA.format = fmt
    , CGNA.sizeMb = sz
    } = do
    format <- parseFormat fmt
    result <- NI.createImage (T.unpack p) format sz
    pure
      CGNA.Session'diskCreate'results
        { CGNA.result = encodeDiskOpResult result
        }

sessionDiskCreateOverlay
  :: CGNA.Parsed CGNA.Session'diskCreateOverlay'params
  -> IO (CGNA.Parsed CGNA.Session'diskCreateOverlay'results)
sessionDiskCreateOverlay
  CGNA.Session'diskCreateOverlay'params
    { CGNA.overlayPath = ov
    , CGNA.backingPath = bk
    , CGNA.backingFormat = bf
    } = do
    format <- parseFormat bf
    result <- NI.createOverlay (T.unpack ov) (T.unpack bk) format
    pure
      CGNA.Session'diskCreateOverlay'results
        { CGNA.result = encodeDiskOpResult result
        }

sessionDiskDelete
  :: CGNA.Parsed CGNA.Session'diskDelete'params
  -> IO (CGNA.Parsed CGNA.Session'diskDelete'results)
sessionDiskDelete
  CGNA.Session'diskDelete'params {CGNA.path = p} = do
    result <- NI.deleteImage (T.unpack p)
    pure
      CGNA.Session'diskDelete'results
        { CGNA.result = encodeDiskOpResult result
        }

sessionDiskResize
  :: CGNA.Parsed CGNA.Session'diskResize'params
  -> IO (CGNA.Parsed CGNA.Session'diskResize'results)
sessionDiskResize
  CGNA.Session'diskResize'params
    { CGNA.path = p
    , CGNA.newSizeMb = sz
    } = do
    result <- NI.resizeImage (T.unpack p) sz
    pure
      CGNA.Session'diskResize'results
        { CGNA.result = encodeDiskOpResult result
        }

sessionDiskRebase
  :: CGNA.Parsed CGNA.Session'diskRebase'params
  -> IO (CGNA.Parsed CGNA.Session'diskRebase'results)
sessionDiskRebase
  CGNA.Session'diskRebase'params
    { CGNA.overlayPath = ov
    , CGNA.newBacking = nb
    , CGNA.newBackingFormat = nbf
    , CGNA.hasNewBacking = hnb
    , CGNA.unsafeUpdate = uu
    } = do
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

sessionDiskClone
  :: CGNA.Parsed CGNA.Session'diskClone'params
  -> IO (CGNA.Parsed CGNA.Session'diskClone'results)
sessionDiskClone
  CGNA.Session'diskClone'params
    { CGNA.sourcePath = src
    , CGNA.destPath = dst
    , CGNA.destFormat = destFormat
    } = do
    result <- NI.cloneImage (T.unpack src) (T.unpack dst) destFormat
    pure
      CGNA.Session'diskClone'results
        { CGNA.result = encodeDiskOpResult result
        }

sessionDiskInspect
  :: CGNA.Parsed CGNA.Session'diskInspect'params
  -> IO (CGNA.Parsed CGNA.Session'diskInspect'results)
sessionDiskInspect
  CGNA.Session'diskInspect'params {CGNA.path = p} = do
    result <- NI.getImageInfo (T.unpack p)
    case result of
      Left err -> throwFailed err
      Right info ->
        pure
          CGNA.Session'diskInspect'results
            { CGNA.info = encodeDiskInspectInfo info
            }
